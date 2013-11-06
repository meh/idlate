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

  defrecord User, [:client, :host, :port, :secure?, :nick, :name, :real_name, :modes, { :channels, [] }] do
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

  defrecord Channel, [:name, :password, :modes, { :users, [] }] do
    defrecord User, [:client, :channel, :modes]
  end

  defmacrop registered?(client, do: body) do
    quote do
      var!(user) = call { :user, :get, unquote(client) }

      if var!(user).registered? do
        unquote(body)
      else
        Error.NotRegistered[]
      end
    end
  end

  defmacrop registered?(client, do: body, else: other) do
    quote do
      var!(user) = call { :user, :get, unquote(client) }

      if var!(user).registered? do
        unquote(body)
      else
        unquote(other)
      end
    end
  end

  defmacrop unregistered?(client, do: body) do
    quote do
      var!(user) = call { :user, :get, unquote(client) }

      if var!(user).registered? do
        Error.AlreadyRegistered[]
      else
        unquote(body)
      end
    end
  end

  defmacrop unregistered?(client, do: body, else: other) do
    quote do
      var!(user) = call { :user, :get, unquote(client) }

      if var!(user).registered? do
        unquote(other)
      else
        unquote(body)
      end
    end
  end

  start do
    { :ok, State[] }
  end

  config _body do
    []
  end

  call { :config, name }, State[config: config] = _state do
    { :ok, Dict.get(config, name), _state }
  end

  call { :user, :new, client }, State[users: users] = state do
    Client.Info[host: host, port: port, secure: secure] = client |> Reagent.Connection.env

    user  = User[client: client, host: host, port: port, secure?: secure]
    state = users |> Dict.put(client, user) |> state.users

    { :ok, state }
  end

  call { :user, :get, name }, State[users: users, nicks: nicks] = _state when name |> is_binary do
    client = nicks |> Dict.get(name |> String.downcase)

    if client do
      { :ok, users |> Dict.get(client), _state }
    else
      { :ok, nil, _state }
    end
  end

  call { :user, :get, client }, State[users: users] = _state do
    { :ok, users |> Dict.get(client), _state }
  end

  call { :user, :update, client, list }, State[users: users] = state do
    user = users |> Dict.get(client)
    user = user.update(list)

    state = users |> Dict.put(client, user) |> state.users

    { :ok, user, state }
  end

  call { :user, :nick, client, name }, State[users: users, nicks: nicks] = state do
    user = Dict.get(users, client)
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

        state = nicks |> Dict.put(new, client) |> state.nicks
        state = users |> Dict.put(client, user.nick(name)) |> state.users

        { :ok, state }
    end
  end

  call { :user, :delete, client }, State[users: users, nicks: nicks] = state do
    user = users |> Dict.get(client)

    if user do
      state = users |> Dict.delete(client) |> state.users
      state = nicks |> Dict.delete(user.nick |> String.downcase) |> state.nicks
    end

    { :ok, state }
  end

  call { :channel, :get, name }, State[channels: channels] = _state do
    { :ok, channels |> Dict.get(name), _state }
  end

  handle :connected, client do
    call { :user, :new, client }

    nil
  end

  handle :disconnected, client do
    call { :user, :delete, client }

    nil
  end

  input "PASS " <> password, _ do
    Event.Password[content: password |> String.strip]
  end

  handle Event.Password[content: password], client do
    unregistered? client do
      if real = call { :config, :password } do
        if password == real do
          IO.puts "the password is right"
        end
      else
        IO.puts "there's no pass dude"
      end

      nil
    end
  end

  input "NICK " <> rest, _ do
    Event.Nick[name: rest |> String.strip]
  end

  handle Event.Nick[name: name], client do
    case call { :user, :nick, client, name } do
      { :error, :in_use } ->
        Error.NicknameInUse[ name: name]

      { :error, :current } ->
        nil

      :ok ->
        nil
    end
  end

  input "USER " <> rest, _ do
    [rest, real_name] = rest |> String.split(":", global: false)
    [name, modes, _]  = rest |> String.strip |> String.split(" ")

    Event.User[name: name, real_name: real_name, modes: modes]
  end

  handle Event.User[name: name, real_name: real_name], client do
    unregistered? client do
      user = call { :user, :update, client, [name: name, real_name: real_name] }

      if user.nick do
        [ Response.Welcome[server: Idlate.name, mask: to_string(user)],
          Response.HostedBy[server: Idlate.name, ip: "0.0.0.0", port: user.port, version: @version],
          Response.ServCreatedOn[created_on: "last thursday"],
          Response.ServInfo[host: Idlate.name, version: @version, user: "NZo", channel: "abcCehiIkKlLmnNoQsStuvVxyz"] ]
      end
    end
  end

  input "PING " <> rest, _ do
    Event.Ping[cookie: rest]
  end

  handle Event.Ping[cookie: cookie], client do
    registered? client do
      Event.Pong[cookie: cookie]
    end
  end

  output Event.Pong[cookie: cookie], _ do
    "PONG #{cookie}"
  end

  input "JOIN 0", _ do
    Event.Part[reason: "Left all channels"]
  end

  input "JOIN " <> rest, _ do
    Enum.map String.split(rest, ","), fn rest ->
      case rest |> String.strip |> String.split(" ") do
        [channel, password] ->
          Event.Join[channel: channel |> String.rstrip, password: password |> String.lstrip]

        [channel] ->
          Event.Join[channel: channel]
      end
    end
  end

  handle Event.Join[channel: channel, password: password], client do
    registered? client do
      channel = call { :channel, :get, channel }
    end
  end

  input "PRIVMSG " <> rest, _ do
    [to, content] = String.split(rest, ":", global: false)

    recipient = case to |> String.rstrip do
      << type :: utf8, _ :: binary >> = name when type in [?&, ?#, ?+, ?!] ->
        { :channel, name }

      name ->
        { :user, name }
    end

    Event.Message[to: recipient, content: content]
  end

  handle Event.Message[to: { :channel, name }, content: content], client do
    registered? client do
      if to = call { :channel, :get, name } do
        to.users |> Enum.map fn client ->
          { client, Event.Message[from: user, to: { :channel, name }, content: content] }
        end
      else
        Error.NoSuchChannel[channel: name]
      end
    end
  end

  handle Event.Message[to: { :user, name }, content: content], client do
    registered? client do
      if to = call { :user, :get, name } do
        { to.client, Event.Message[from: user, to: { :user, name }, content: content] }
      else
        Error.NoSuchNick[nick: name]
      end
    end
  end

  output Event.Message[from: user, to: { _, name }, content: content], client do
    ":#{user} PRIVMSG #{name} :#{content}"
  end

  require Response

  Enum.each Response.names, fn name ->
    output Response.unquote(name)[] = record, client do
      user = call { :user, :get, client }

      Numeric.to_string(Idlate.name, user.nick, record)
    end
  end

  require Error

  Enum.each Error.names, fn name ->
    output Error.unquote(name)[] = record, client do
      user = call { :user, :get, client }

      Numeric.to_string(Idlate.name, user.nick, record)
    end
  end
end
