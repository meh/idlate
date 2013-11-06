defmodule Idlate.Client do
  use Reagent.Behaviour

  def start(conn) do
    :gen_server.start __MODULE__, [conn], []
  end

  alias Idlate.Event

  use GenServer.Behaviour

  defrecord Info, ip: nil, host: nil, port: nil, secure: nil

  def init([connection]) do
    Process.flag :trap_exit, true

    { ip, _port } = connection |> Socket.remote!
    host         = case Socket.Host.by_address(ip) do
      { :ok, Socket.Host[name: name] } ->
        name

      { :error, _ } ->
        Socket.Address.format(ip)
    end

    listener = connection |> Reagent.Connection.listener
    port     = listener |> Reagent.Listener.port
    secure   = listener |> Reagent.Listener.secure?

    connection |> Reagent.Connection.env(Info[ip: ip, host: host, port: port, secure: secure])

    { :ok, connection }
  end

  def handle_info({ Reagent, :ack }, connection) do
    connection |> Socket.packet! :line

    :gen_server.cast Idlate, { connection, :connected }

    Event.trigger(connection, :connected) |> Process.link

    { :noreply, connection }
  end

  def handle_info({ :tcp, _, line }, connection) do
    Event.parse(connection, line |> String.replace(%r/\r?\n$/, ""))
      |> Process.link

    { :noreply, connection }
  end

  def handle_info({ :tcp_closed, _ }, connection) do
    :gen_server.cast Idlate, { connection, :disconnected }

    Event.trigger(connection, :disconnected)

    { :noreply, connection }
  end

  def handle_info({ :EXIT, _pid, reason }, connection) do
    connection |> Socket.active! :once

    if reason != :normal do
      :error_logger.error_report reason
    end

    { :noreply, connection }
  end

  def handle_cast(:shutdown, _connection) do
    { :stop, :normal, _connection }
  end

  def handle_cast({ :send, data }, connection) do
    Enum.each List.wrap(data), &Socket.Stream.send!(connection, [&1, "\r\n"])

    { :noreply, connection }
  end

  def handled(pid) do
    :gen_server.cast(pid, :handled)
  end
end
