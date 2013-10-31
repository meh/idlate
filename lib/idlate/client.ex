defmodule Idlate.Client do
  use Reagent.Behaviour

  def start(conn) do
    :gen_server.start __MODULE__, [conn], []
  end

  use GenServer.Behaviour

  def init([connection]) do
    { :ok, connection }
  end

  def handle_info({ Reagent, :ack }, connection) do
    connection |> Socket.packet! :line
    connection |> Socket.active! :once

    :gen_server.cast Idlate, { Process.self, :connected }

    { :noreply, connection }
  end

  def handle_info({ :tcp, _, line }, _connection) do
    :gen_server.cast Idlate, { Process.self, :sent, String.rstrip(line) }

    { :noreply, _connection }
  end

  def handle_info({ :tcp_closed, reason }, _connection) do
    :gen_server.cast Idlate, { Process.self, :disconnected, reason }

    { :noreply, _connection }
  end

  def handle_cast(:shutdown, _connection) do
    { :stop, :normal, _connection }
  end

  def handle_cast(:handled, connection) do
    connection |> Socket.active! :once

    { :noreply, connection }
  end

  def handle_cast({ :send, data }, connection) do
    Enum.each List.wrap(data), &Socket.Stream.send!(connection, [&1, "\r\n"])

    { :noreply, connection }
  end

  def send(pid, data) do
    :gen_server.cast(pid, { :send, data })
  end

  def handled(pid) do
    :gen_server.cast(pid, :handled)
  end
end
