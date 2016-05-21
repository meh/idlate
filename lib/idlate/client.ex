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

defmodule Idlate.Client do
  use Reagent.Behaviour

  def start(conn) do
    GenServer.start __MODULE__, [conn]
  end

  alias Idlate.Event
  use GenServer

  defmodule Info do
    defstruct [:ip, :host, :port, :secure]
  end

  def init([connection]) do
    Process.flag :trap_exit, true

    { :ok, { connection, Idlate.Connection.load(connection) } }
  end

  def handle_info({ Reagent, :ack }, { connection, details } = state) do
    connection |> Socket.packet!(:line)

    :gen_server.cast Idlate, { :connected, connection, details }

    Event.trigger(connection.id, :connected) |> Process.link

    { :noreply, state }
  end

  def handle_info({ :tcp, _, line }, { connection, _details } = state) do
    Event.parse(connection.id, line |> String.replace(~r/\r?\n$/, ""))
      |> Process.link

    { :noreply, state }
  end

  def handle_info({ :tcp_closed, _ }, { connection, _details } = state) do
    :gen_server.cast Idlate, { :disconnected, connection }

    Event.trigger(connection.id, :disconnected)

    { :noreply, state }
  end

  def handle_info({ :EXIT, _pid, reason }, { connection, _details } = state) do
    connection |> Socket.active!(:once)

    if reason != :normal do
      Logger.bare_log(:error, reason)
    end

    { :noreply, state }
  end

  def handle_cast(:shutdown, state) do
    { :stop, :normal, state }
  end
end
