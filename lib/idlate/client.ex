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

  # Handle the Reagent initialization.
  def handle_info({ Reagent, :ack }, %{stream: stream, details: details} = state) do
    # IRC is a line based protocol.
    stream |> Socket.packet!(:line)

    # Tell the server a client has connected.
    GenServer.cast Idlate, { :connected, stream, details }

    # Trigger the connected event synchronously so plugins can handle it.
    Event.trigger!(stream.id, :connected)

    # The socket is active once because event handling spawns a process to
    # handle the pipeline, and we want to handle messages from a single client
    # serially.
    stream |> Socket.active!(:once)

    { :noreply, state }
  end

  # Receiving a line from the client.
  def handle_info({ :tcp, _, line }, %{stream: stream} = state) do
    # Spawn the event handling, the process is linked because we reactivate the
    # socket on its exit.
    Process.link Event.parse(stream.id, line |> String.replace(~r/\r?\n$/, ""))

    { :noreply, state }
  end

  # The client has died for a reason or another.
  def handle_info({ :tcp_closed, _ }, %{stream: stream} = state) do
    # Trigger the disconnected event synchronously.
    Event.trigger!(stream.id, :disconnected)

    # Tell the server the client has disconnected.
    GenServer.cast Idlate, { :disconnected, stream }

    { :noreply, state }
  end

  # The previous event pipeline has ended.
  def handle_info({ :EXIT, _pid, reason }, %{stream: stream} = state) do
    # Reactivate the socket so we can receive another message and handle it.
    stream |> Socket.active!(:once)

    # If the event pipeline failed, log the error.
    if reason != :normal do
      Logger.bare_log(:error, reason)
    end

    { :noreply, state }
  end

  # Helper to cleanly shutdown a client.
  #
  # TODO: deal with a forced disconnection.
  def handle_cast(:shutdown, state) do
    { :stop, :normal, state }
  end
end
