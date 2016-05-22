# Copyleft (ɔ) meh. - http://meh.schizofreni.co
#
# This file is part of idlate - https://github.com/meh/idlate
#
# idlate is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# idlate is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with idlate. If not, see <http://www.gnu.org/licenses/>.

defmodule Idlate do
  use Application

  # See http://elixir-lang.org/docs/stable/Application.Behaviour.html
  # for more information on OTP Applications
  def start(_type, args) do
    GenServer.start_link __MODULE__, args, name: Idlate
  end

  use GenServer
  use Data

  alias Idlate.Supervisor
  alias Idlate.Config

  @state %{supervisor: nil, name: nil, plugins: [], clients: %{}}

  def init(options) do
    case Supervisor.start_link do
      { :ok, pid } ->
        if path = options[:config] do
          GenServer.cast Idlate, { :load, path }
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
        plugins = Seq.sort [{ module, priority } | plugins], &(elem(&1, 1) < elem(&2, 1))
        state   = %{state | plugins: plugins}

      { :error, reason } ->
        IO.inspect "HUEHUEHUEHUE #{inspect reason}"
    end

    { :noreply, state }
  end

  def handle_cast({ :connected, client, details }, %{clients: clients} = state) do
    state = %{state | clients: clients |> Dict.put(client.id, { client, details })}

    { :noreply, state }
  end

  def handle_cast({ :disconnected, client }, %{clients: clients} = state) do
    { :noreply, %{state | clients: clients |> Dict.delete(client.id)} }
  end

  # TODO: optimize this since it's called on every input line
  def handle_call(:plugins, _from, %{plugins: plugins} = state) do
    { :reply, plugins |> Dict.keys, state }
  end

  def handle_call(:name, _from, %{name: name} = state) do
    { :reply, name, state }
  end

  def handle_call({ :connection, :details, id }, _from, %{clients: clients} = state) do
    case clients |> Dict.get(id) do
      nil ->
        { :reply, nil, state }

      { _, details } ->
        { :reply, details, state }
    end
  end

  def handle_call({ :connection, :stream, id }, _from, %{clients: clients} = state) do
    case clients |> Dict.get(id) do
      nil ->
        { :reply, nil, state }

      { stream, _ } ->
        { :reply, stream, state }
    end
  end

  def plugins do
    GenServer.call(Idlate, :plugins)
  end

  def name do
    GenServer.call(Idlate, :name)
  end

  def connection(id, what) when id |> is_reference do
    GenServer.call(Idlate, { :connection, what, id })
  end

  def reply(id, output, plugins \\ Idlate.plugins) do
    Idlate.Event.reply(id, output, plugins)
  end
end
