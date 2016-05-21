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

    { ip, _port } = connection |> Socket.remote!
    host          = case Socket.Host.by_address(ip) do
      { :ok, %Socket.Host{name: name} } ->
        name

      { :error, _ } ->
        Socket.Address.format(ip)
    end

    listener = connection |> Reagent.Connection.listener
    port     = listener |> Reagent.Listener.port
    secure   = listener |> Reagent.Listener.secure?

    connection |> Reagent.Connection.env(%Info{ip: ip, host: host, port: port, secure: secure})

    { :ok, connection }
  end

  def handle_info({ Reagent, :ack }, connection) do
    connection |> Socket.packet!(:line)

    :gen_server.cast Idlate, { connection, :connected }

    Event.trigger(connection, :connected) |> Process.link

    { :noreply, connection }
  end

  def handle_info({ :tcp, _, line }, connection) do
    Event.parse(connection, line |> String.replace(~r/\r?\n$/, ""))
      |> Process.link

    { :noreply, connection }
  end

  def handle_info({ :tcp_closed, _ }, connection) do
    :gen_server.cast Idlate, { connection, :disconnected }

    Event.trigger(connection, :disconnected)

    { :noreply, connection }
  end

  def handle_info({ :EXIT, _pid, reason }, connection) do
    connection |> Socket.active!(:once)

    if reason != :normal do
      :error_logger.error_report reason
    end

    { :noreply, connection }
  end

  def handle_cast(:shutdown, connection) do
    { :stop, :normal, connection }
  end

  def handle_cast({ :send, data }, connection) do
    Enum.each List.wrap(data), &Socket.Stream.send!(connection, [&1, "\r\n"])

    { :noreply, connection }
  end

  def handled(pid) do
    :gen_server.cast(pid, :handled)
  end
end
