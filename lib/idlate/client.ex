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

  @state %{stream:  nil,
           details: nil}

  def init([stream]) do
    Process.flag :trap_exit, true

    { :ok, %{@state | stream: stream, details: Idlate.Connection.load(stream)} }
  end

  def handle_info({ Reagent, :ack }, %{stream: stream, details: details} = state) do
    stream |> Socket.packet!(:line)

    GenServer.cast Idlate, { :connected, stream, details }
    Event.trigger!(stream.id, :connected)

    stream |> Socket.active!(:once)

    { :noreply, state }
  end

  def handle_info({ :tcp, _, line }, %{stream: stream} = state) do
    Event.parse(stream.id, line |> String.replace(~r/\r?\n$/, ""))
      |> Process.link

    { :noreply, state }
  end

  def handle_info({ :tcp_closed, _ }, %{stream: stream} = state) do
    Event.trigger!(stream.id, :disconnected)
    GenServer.cast Idlate, { :disconnected, stream }

    { :noreply, state }
  end

  def handle_info({ :EXIT, _pid, reason }, %{stream: stream} = state) do
    stream |> Socket.active!(:once)

    if reason != :normal do
      Logger.bare_log(:error, reason)
    end

    { :noreply, state }
  end

  def handle_cast(:shutdown, state) do
    { :stop, :normal, state }
  end
end
