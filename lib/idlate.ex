defmodule Idlate do
  use Application.Behaviour

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, args) do
    :gen_server.start_link { :local, __MODULE__ }, __MODULE__, args, []
  end

  use GenServer.Behaviour

  alias Data.Set

  alias Idlate.Supervisor
  alias Idlate.Event
  alias Idlate.Config

  defrecord State, supervisor: nil, name: nil, plugins: [], clients: HashSet.new

  def init(options) do
    case Supervisor.start_link do
      { :ok, pid } ->
        if path = options[:config] do
          :gen_server.cast Idlate, { :load, path }
        end

        { :ok, State[supervisor: pid] }

      { :error, reason } ->
        { :error, reason }
    end
  end

  def handle_cast({ :load, path }, _state) do
    Config.load(path)

    { :noreply, _state }
  end

  def handle_cast({ :name, name }, state) do
    { :noreply, state.name(name) }
  end

  def handle_cast({ :listen, listener }, State[supervisor: supervisor] = _state) do
    supervisor |> Supervisor.listen(listener)

    { :noreply, _state }
  end

  def handle_cast({ :plugin, module, configuration }, State[supervisor: supervisor, plugins: plugins] = state) do
    case supervisor |> Supervisor.plugin(module, configuration) do
      { :ok, priority } ->
        plugins = Enum.sort [{ module, priority } | plugins], &(elem(&1, 1) < elem(&2, 1))
        state   = state.plugins(plugins)

      { :error, reason } ->
        IO.inspect "HUEHUEHUEHUE #{inspect reason}"
    end

    { :noreply, state }
  end

  def handle_cast({ client, :connected }, State[clients: clients] = state) do
    state = clients |> Set.add(client) |> state.clients

    Event.trigger(client, { :connected, client }, false)

    { :noreply, state }
  end

  def handle_cast({ client, :sent, line }, _state) do
    Event.parse(client, line)

    { :noreply, _state }
  end

  def handle_cast({ client, :disconnected, reason }, State[clients: clients] = state) do
    state = Set.delete(clients, client) |> state.clients

    Event.trigger(client, { :disconnected, client, reason })

    { :noreply, state }
  end

  # TODO: optimize this since it's called on every input line
  def handle_call(:plugins, _from, State[plugins: plugins] = _state) do
    { :reply, Enum.map(plugins, &elem(&1, 0)), _state }
  end

  def handle_call(:name, _from, State[name: name] = _state) do
    { :reply, name, _state }
  end

  def plugins do
    :gen_server.call(Idlate, :plugins)
  end

  def name do
    :gen_server.call(Idlate, :name)
  end
end
