defmodule Idlate do
  use Application

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, args) do
    GenServer.start_link __MODULE__, args
  end

  use GenServer

  alias Idlate.Supervisor
  alias Idlate.Config

  @state %{supervisor: nil, name: nil, plugins: HashSet.new, clients: %{}}

  def init(options) do
    case Supervisor.start_link do
      { :ok, pid } ->
        if path = options[:config] do
          :gen_server.cast Idlate, { :load, path }
        end

        { :ok, %{@state | supervisor: pid} }

      { :error, reason } ->
        { :error, reason }
    end
  end

  def handle_cast({ :load, path }, state) do
    Config.load(path)

    { :noreply, state }
  end

  def handle_cast({ :name, name }, state) do
    { :noreply, %{state | name: name} }
  end

  def handle_cast({ :listen, listener }, %{supervisor: supervisor} = state) do
    supervisor |> Supervisor.listen(listener)

    { :noreply, state }
  end

  def handle_cast({ :plugin, module, configuration }, %{supervisor: supervisor, plugins: plugins} = state) do
    case supervisor |> Supervisor.plugin(module, configuration) do
      { :ok, priority } ->
        plugins = Enum.sort [{ module, priority } | plugins], &(elem(&1, 1) < elem(&2, 1))
        state   = %{state | plugins: plugins}

      { :error, reason } ->
        IO.inspect "HUEHUEHUEHUE #{inspect reason}"
    end

    { :noreply, state }
  end

  def handle_cast({ client, :connected }, %{clients: clients} = state) do
    state = %{state | clients: Set.put(clients, client)}

    { :noreply, state }
  end

  def handle_cast({ client, :disconnected }, %{clients: clients} = state) do
    { :noreply, %{state | clients: Map.delete(clients, client)} }
  end

  # TODO: optimize this since it's called on every input line
  def handle_call(:plugins, _from, %{plugins: plugins} = state) do
    { :reply, Enum.map(plugins, &elem(&1, 0)), state }
  end

  def handle_call(:name, _from, %{name: name} = state) do
    { :reply, name, state }
  end

  def plugins do
    :gen_server.call(Idlate, :plugins)
  end

  def name do
    :gen_server.call(Idlate, :name)
  end
end
