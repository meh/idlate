defmodule Idlate do
  use Application.Behaviour

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    :gen_server.start_link { :local, __MODULE__ }, __MODULE__, [], []
  end

  use GenServer.Behaviour

  defrecord State, supervisor: nil, connections: HashDict.new

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

  def handle_cast({ connection, :connected }, State[connections: connections] = state) do
    state = connections |> Dict.put(Idlate.Client.id(connection), connection) |> state.connections

    IO.inspect state

    { :noreply, state }
  end

  def handle_cast({ :handle, client, line }, _state) do
    IO.inspect { client, line }

    { :noreply, _state }
  end
end
