defmodule Idlate.Client do
  use Reagent.Behaviour

  def start(conn) do
    :gen_server.start __MODULE__, [conn.listener.env, conn], []
  end

  use GenServer.Behaviour

  def init([server, connection]) do
    { :ok, { server, connection, HashDict.new } }
  end

  def handle_info({ Reagent, :ack }, { server, connection, _data } = _state) do
    connection |> Socket.packet! :line
    connection |> Socket.active! :once

    :gen_server.cast server, { Process.self, :connected }

    { :noreply, _state }
  end

  def handle_info({ :tcp, _, line }, { server, _connection, _data } = _state) do
    :gen_server.cast server, { Process.self, :handle, String.rstrip(line) }

    { :noreply, _state }
  end

  def handle_info({ :tcp_closed, _ }, { server, _connection, _data } = _state) do
    :gen_server.cast server, { Process.self, :disconnected }

    { :stop, :normal, _state }
  end

  def handle_cast(:handled, { _server, connection, _data } = _state) do
    connection |> Socket.active! :once
  end

  def handle_call(:id, _from, { _server, connection, data } = _state) do
    { :reply, data |> Dict.get(:nick, connection.id), _state }
  end

  def id(pid) do
    :gen_server.call(pid, :id)
  end
end
