defmodule Idlate.RFC281X do
  defmodule Numeric do
    def to_string(server, client, record) do
      ":#{server} #{pad((record |> elem(0)).number)} #{client || "*"} #{to_string(record)}"
    end

    def pad(n) when n < 10,   do: "00#{n}"
    def pad(n) when n < 100,  do: "0#{n}"
    def pad(n) when n < 1000, do: "#{n}"
  end

  use Idlate.Plugin

  alias Data.Dict

  alias Idlate.Client

  alias __MODULE__.Event
  alias __MODULE__.Response
  alias __MODULE__.Error

  defrecord State, config: HashDict.new, users: HashDict.new, nicks: HashDict.new, channels: HashDict.new

  defrecord User, [:id, :nick, :name, :real_name, :host, :modes] do
    def registered?(User[nick: nick, name: name]) when nick |> nil? or name |> nil? do
      false
    end

    def registered?(_) do
      true
    end

    defimpl String.Chars do
      def to_string(User[nick: nick, name: name, host: host]) do
        "#{nick}!#{name}@#{host}"
      end
    end
  end

  defrecord Channel, [:name, :modes, :users] do
    defrecord User, [:user, :channel, :modes]
  end

  start do
    { :ok, State[] }
  end

  config body do
    []
  end

  call { :config, name }, State[config: config] = _state do
    { :ok, Dict.get(config, name), _state }
  end

  call { :user, :new, pid }, State[users: users] = state do
    user  = User[id: pid, host: Client.host(pid)]
    state = users |> Dict.put(pid, user) |> state.users

    { :ok, state }
  end

  call { :user, :get, name }, State[users: users, nicks: nicks] = _state when name |> is_binary do
    id = nicks |> Dict.get(name |> String.downcase)

    if id do
      { :ok, users |> Dict.get(id), _state }
    else
      { :ok, nil, _state }
    end
  end

  call { :user, :get, id }, State[users: users] = _state when id |> is_pid do
    { :ok, users |> Dict.get(id), _state }
  end

  call { :user, :update, id, list }, State[users: users] = state do
    state = users |> Dict.update(id, &(&1.update(list))) |> state.users

    { :ok, state }
  end

  call { :user, :nick, id, name }, State[users: users, nicks: nicks] = state do
    user = Dict.get(users, id)
    old  = user.nick && String.downcase(user.nick)
    new  = String.downcase(name)

    cond do
      old && old == new ->
        { :error, :current, state }

      nicks |> Dict.has_key?(new) ->
        { :error, :in_use, state }

      true ->
        if old do
          state = nicks |> Dict.delete(old) |> state.nicks
        end

        state = nicks |> Dict.put(new, id) |> state.nicks
        state = users |> Dict.put(id, user.nick(name)) |> state.users

        { :ok, state }
    end
  end

  call { :user, :delete, id }, State[users: users, nicks: nicks] = state do
    user = users |> Dict.get(id)

    if user do
      state = users |> Dict.delete(id) |> state.users
      state = nicks |> DIct.delete(user.nick |> String.downcase) |> state.nicks
    end

    { :ok, state }
  end

  call { :channel, :get, name }, State[channels: channels] = _state do
    { :ok, channels |> Dict.get(name), _state }
  end

  handle { :connected, client } do
    call { :user, :new, client }

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

      { :error, :current } ->
        nil

      :ok ->
        nil
    end
  end

  input "USER " <> rest do
    [rest, real_name] = rest |> String.split(":", global: false)
    [name, modes, _]  = rest |> String.strip |> String.split(" ")

    Event.User[name: name, real_name: real_name, modes: modes]
  end

  handle Event.User[client: id, name: name, real_name: real_name] do
    call { :user, :update, id, [name: name, real_name: real_name] }

    user = call { :user, :get, id }

    if user.nick do
      [ Response.Welcome[client: id, server: Idlate.name, mask: to_string(user)],
        Response.HostedBy[client: id, server: Idlate.name, version: @version],
        Response.ServCreatedOn[client: id, created_on: "last thursday"] ]
    end
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

  require Response

  Enum.each Response.names, fn name ->
    output Response.unquote(name)[client: id] = record do
      user = call { :user, :get, id }

      Numeric.to_string(Idlate.name, user.nick, record)
    end
  end

  require Error

  Enum.each Error.names, fn name ->
    output Error.unquote(name)[client: id] = record do
      user = call { :user, :get, id }

      Numeric.to_string(Idlate.name, user.nick, record)
    end
  end
end
