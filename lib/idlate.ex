defmodule Idlate do
  use Application.Behaviour

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, _args) do
    :gen_server.start_link { :local, __MODULE__ }, __MODULE__, [], []
  end

  use GenServer.Behaviour

  alias Data.Set

  defrecord State, supervisor: nil, plugins: [], clients: HashSet.new

  def init([]) do
    case Idlate.Supervisor.start_link do
      { :ok, pid } ->
        { :ok, State[supervisor: pid] }

      { :error, reason } ->
        { :error, reason }
    end
  end

  def handle_cast({ :listen, listener }, State[supervisor: supervisor] = _state) do
    supervisor |> Idlate.Supervisor.listen(listener)

    { :noreply, _state }
  end

  def handle_cast({ :plugin, module, priority, configuration }, State[plugins: plugins] = state) do
    case module.start_link(configuration) do
      { :ok, _ } ->
        state = [{ module, priority } | plugins] |> state.plugins

      { :error, reason } ->
        IO.inspect "HUEHUEHUEHUE #{inspect reason}"
    end

    { :noreply, state }
  end

  def handle_cast({ client, :connected }, State[clients: clients] = state) do
    state = clients |> Set.add(client) |> state.clients

    Idlate.Event.trigger(client, { :connected, client })

    { :noreply, state }
  end

  def handle_cast({ client, :sent, line }, _state) do
    Idlate.Event.parse(client, line)

    { :noreply, _state }
  end

  def handle_cast({ client, :disconnected, reason }, State[clients: clients] = state) do
    state = Set.delete(clients, client) |> state.clients

    Idlate.Event.trigger(client, { :disconnected, client, reason })

    { :noreply, state }
  end

  # TODO: optimize this since it's called on every input line
  def handle_call(:plugins, _from, State[plugins: plugins] = _state) do
    plugins = plugins |> Enum.sort(&(elem(&1, 1) > elem(&2, 1))) |> Enum.map(&elem(&1, 0))

    { :reply, plugins, _state }
  end

  def plugins do
    :gen_server.call(Idlate, :plugins)
  end
end
