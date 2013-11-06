defmodule Idlate.Client do
  use Reagent.Behaviour

  def start(conn) do
    :gen_server.start __MODULE__, [conn], []
  end

  alias Idlate.Event

  use GenServer.Behaviour

  defrecord State, connection: nil, ip: nil, host: nil, port: nil, secure: nil

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

    { :ok, State[connection: connection, ip: ip, host: host, port: port, secure: secure] }
  end

  def handle_info({ Reagent, :ack }, State[connection: connection] = _state) do
    connection |> Socket.packet! :line

    :gen_server.cast Idlate, { Process.self, :connected }

    Event.trigger(Process.self, :connected) |> Process.link

    { :noreply, _state }
  end

  def handle_info({ :tcp, _, line }, _state) do
    Event.parse(Process.self, line |> String.replace(%r/\r?\n$/, "")) |> Process.link

    { :noreply, _state }
  end

  def handle_info({ :tcp_closed, _ }, _state) do
    :gen_server.cast Idlate, { Process.self, :disconnected }

    Event.trigger(Process.self, :disconnected)

    { :noreply, _state }
  end

  def handle_info({ :EXIT, _pid, reason }, State[connection: connection] = _state) do
    connection |> Socket.active! :once

    if reason != :normal do
      :error_logger.error_report reason
    end

    { :noreply, _state }
  end

  def handle_cast(:shutdown, _state) do
    { :stop, :normal, _state }
  end

  def handle_cast({ :send, data }, State[connection: connection] = _state) do
    Enum.each List.wrap(data), &Socket.Stream.send!(connection, [&1, "\r\n"])

    { :noreply, _state }
  end

  def send(pid, data) do
    :gen_server.cast(pid, { :send, data })
  end

  def handled(pid) do
    :gen_server.cast(pid, :handled)
  end

  def handle_call(:ip, _from, State[ip: ip] = _state) do
    { :reply, ip, _state }
  end

  def handle_call(:host, _from, State[host: host] = _state) do
    { :reply, host, _state }
  end

  def handle_call(:port, _from, State[port: port] = _state) do
    { :reply, port, _state }
  end

  def handle_call(:secure?, _from, State[secure: secure] = _state) do
    { :reply, secure, _state }
  end

  def ip(pid) do
    :gen_server.call(pid, :ip)
  end

  def host(pid) do
    :gen_server.call(pid, :host)
  end

  def port(pid) do
    :gen_server.call(pid, :port)
  end

  def secure?(pid) do
    :gen_server.call(pid, :secure?)
  end
end
