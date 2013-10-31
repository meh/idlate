defmodule Idlate.RFC281X do
  defmodule Numeric do
    def to_string(server, client, record) do
      ":#{server} #{pad((record |> elem(0)).number)} #{client} #{to_string(record)}"
    end

    def pad(n) when n < 10,   do: "00#{n}"
    def pad(n) when n < 100,  do: "0#{n}"
    def pad(n) when n < 1000, do: "#{n}"
  end

  use Idlate.Plugin

  alias __MODULE__.Event
  alias __MODULE__.Parse

  defrecord State, users: HashDict.new, channels: HashDict.new

  defrecord User, [:id, :nick, :name, :real_name, :host, :modes]
  defrecord Channel, [:name, :modes, :users] do
    defrecord User, [:user, :channel, :modes]
  end

  start do
    { :ok, State[] }
  end

  call { :channel, name }, State[channels: channels] = _state do
    { :ok, channels |> Dict.get(name), _state }
  end

  call { :user, name }, State[users: users] = _state do
    { :ok, users |> Dict.get(name), _state }
  end

  call { :add, :user, pid }, State[users: users] = state do
    state = users |> Dict.put(pid, User[id: pid]) |> state.users

    { :ok, state }
  end

  call { :remove, :user, pid }, State[users: users] = state do
    state = users |> Dict.delete(pid) |> state.users

    { :ok, state }
  end

  handle { :connected, client } do
    call { :add, :user, client }
  end

  handle { :disconnected, client } do
    call { :remove, :user, client }
  end

  input "PASS " <> password do
    Event.Password[content: password]
  end

  handle Event.Password[content: password] do
    IO.inspect password

    nil
  end

  input "NICK " <> rest do
    Event.Nick[name: rest]
  end

  input "USER " <> rest do
    [rest, real_name] = String.split(rest, ":", global: false)
    [name, modes, _]  = String.split(rest, " ")

    User[name: name, real_name: real_name, modes: modes]
  end

  input "PRIVMSG " <> rest do
    [to, content] = String.split(rest, ":", global: false)

    recipient = case to |> String.rstrip do
      << type :: utf8, _ :: binary >> = name) when type in [?&, ?#, ?+, ?!] ->
        call { :channel, name }

      name ->
        call { :user, name }
    end
    Event.Message[to: recipient, content: content]
  end
end
