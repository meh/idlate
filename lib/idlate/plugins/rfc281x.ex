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

  alias Data.Dict

  alias __MODULE__.Event
  alias __MODULE__.Response
  alias __MODULE__.Error
  alias __MODULE__.Parse

  defrecord State, config: HashDict.new, users: HashDict.new, channels: HashDict.new

  defrecord User, [:id, :nick, :name, :real_name, :host, :modes] do
    def registered?(User[nick: nick, name: name]) when nick |> nil? or name |> nil? do
      false
    end

    def registered?(_) do
      true
    end
  end

  defrecord Channel, [:name, :modes, :users] do
    defrecord User, [:user, :channel, :modes]
  end

  start do
    { :ok, State[] }
  end

  config body do
    IO.inspect body

    []
  end

  call { :config, name }, State[config: config] = _state do
    { :ok, Dict.get(config, name), _state }
  end

  call { :user, :put, pid }, State[users: users] = state do
    state = users |> Dict.put(pid, User[id: pid]) |> state.users

    { :ok, state }
  end

  call { :user, :nick, pid, name }, State[users: users] = state do
    if users |> Dict.has_key?(name) do
      { :error, :in_use, state }
    else
      user = Dict.get(users, pid)

      if old = user.nick do
        state = users |> Dict.delete(old) |> state.users
      end

      state = users |> Dict.put(name, users |> Dict.get(pid)) |> state.users

      { :ok, state }
    end
  end

  call { :user, :delete, pid }, State[users: users] = state do
    state = users |> Dict.delete(pid) |> state.users

    { :ok, state }
  end

  call { :user, :get, name }, State[users: users] = _state do
    { :ok, users |> Dict.get(name), _state }
  end

  call { :channel, :get, name }, State[channels: channels] = _state do
    { :ok, channels |> Dict.get(name), _state }
  end

  handle { :connected, client } do
    call { :user, :put, client }

    nil
  end

  handle { :disconnected, client } do
    call { :user, :delete, client }

    nil
  end

  input "PASS " <> password do
    Event.Password[content: password |> String.strip]
  end

  handle Event.Password[content: password] do
    if real = call { :config, :password } do
      if password == real do
        IO.puts "the password is right"
      end
    else
      IO.puts "there's no pass dude"
    end
  end

  input "NICK " <> rest do
    Event.Nick[name: rest |> String.strip]
  end

  handle Event.Nick[client: client, name: name] do
    case call { :user, :nick, client, name } do
      { :error, :in_use } ->
        Error.NicknameInUse[client: client, name: name]

      :ok ->
        nil
    end
  end

  output Error.NicknameInUse[client: id] = record do
    user = call { :user, :get, id }

    Numeric.to_string(Idlate.name, user.nick || "*", record)
  end

  input "USER " <> rest do
    [rest, real_name] = String.split(rest, ":", global: false)
    [name, modes, _]  = String.split(rest, " ")

    Event.User[name: name, real_name: real_name, modes: modes]
  end

  handle Event.User[name: name, real_name: real_name] do

  end

  input "PRIVMSG " <> rest do
    [to, content] = String.split(rest, ":", global: false)

    recipient = case to |> String.rstrip do
      << type :: utf8, _ :: binary >> = name when type in [?&, ?#, ?+, ?!] ->
        call { :channel, :get, name }

      name ->
        call { :user, :get, name }
    end

    Event.Message[to: recipient, content: content]
  end
end
