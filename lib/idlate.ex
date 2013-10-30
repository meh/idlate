defmodule Idlate do
  use Application.Behaviour

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    :gen_server.start_link { :local, __MODULE__ }, __MODULE__, [], []
  end

  use GenServer.Behaviour

  defrecord State, supervisor: nil, clients: HashDict.new

  def init([]) do
    case Idlate.Supervisor.start_link do
      { :ok, pid } ->
        { :ok, State[supervisor: pid] }

      { :error, reason } ->
        { :error, reason }
    end
  end

  def handle_cast({ :listen, listener }, State[supervisor: supervisor] = _state) do
    supervisor |> Idlate.Supervisor.listen(Process.self, listener)

    { :noreply, _state }
  end

  def handle_cast({ client, :connected }, State[clients: clients] = state) do
    state = clients |> Dict.put(Idlate.Client.id(client), client) |> state.clients

    { :noreply, state }
  end

  def handle_cast({ client, :disconnected }, State[clients: clients] = state) do
    state = clients |> Dict.delete(Idlate.Client.id(client)) |> state.clients

    { :noreply, state }
  end

  def handle_cast({ client, :handle, line }, _state) do
    Line.start(Process.self, client, line)

    { :noreply, _state }
  end
end
