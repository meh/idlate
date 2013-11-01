defmodule Idlate.Client do
  use Reagent.Behaviour

  def start(conn) do
    :gen_server.start __MODULE__, [conn], []
  end

  use GenServer.Behaviour

  defrecord State, connection: nil, ip: nil, port: nil, host: nil

  def init([connection]) do
    { ip, port } = connection |> Socket.remote!
    host         = case Socket.Host.by_address(ip) do
      { :ok, Socket.Host[name: name] } ->
        name

      { :error, _ } ->
        Socket.Address.format(ip)
    end

    { :ok, State[connection: connection, ip: ip, port: port, host: host] }
  end

  def handle_info({ Reagent, :ack }, State[connection: connection] = _state) do
    connection |> Socket.packet! :line

    :gen_server.cast Idlate, { Process.self, :connected }

    { :noreply, _state }
  end

  def handle_info({ :tcp, _, line }, _state) do
    :gen_server.cast Idlate, { Process.self, :sent, String.rstrip(line) }

    { :noreply, _state }
  end

  def handle_info({ :tcp_closed, reason }, _state) do
    :gen_server.cast Idlate, { Process.self, :disconnected, reason }

    { :noreply, _state }
  end

  def handle_cast(:shutdown, _state) do
    { :stop, :normal, _state }
  end

  def handle_cast(:handled, State[connection: connection] = _state) do
    connection |> Socket.active! :once

    { :noreply, _state }
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

  def ip(pid) do
    :gen_server.call(pid, :ip)
  end

  def host(pid) do
    :gen_server.call(pid, :host)
  end
end
