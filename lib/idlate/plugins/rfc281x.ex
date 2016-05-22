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

defmodule Idlate.RFC281X do
  defmodule Numeric do
    def to_string(server, client, %{__struct__: module} = value) do
      ":#{server} #{pad(module.number)} #{client || "*"} #{module.to_string(value)}"
    end

    def pad(n) when n < 10,   do: "00#{n}"
    def pad(n) when n < 100,  do: "0#{n}"
    def pad(n) when n < 1000, do: "#{n}"
  end

  use Idlate.Plugin
  use Data

  alias Idlate.Connection

  alias __MODULE__.Numeric
  alias __MODULE__.Event
  alias __MODULE__.Response
  alias __MODULE__.Error
  alias __MODULE__.Channel
  alias __MODULE__.User

  @state %{config:   %{},
           users:    %{},
           nicks:    %{},
           activity: %{},
           channels: %{}}

  defmacrop registered?(id, do: body) do
    quote do
      var!(user) = call { :user, :get, unquote(id) }

      if var!(user) |> User.registered? do
        unquote(body)
      else
        %Error.NotRegistered{}
      end
    end
  end

  defmacrop registered?(id, do: body, else: other) do
    quote do
      var!(user) = call { :user, :get, unquote(id) }

      if var!(user) |> User.registered? do
        unquote(body)
      else
        unquote(other)
      end
    end
  end

  defmacrop unregistered?(id, do: body) do
    quote do
      var!(user) = call { :user, :get, unquote(id) }

      if var!(user) |> User.registered? do
        %Error.AlreadyRegistered{}
      else
        unquote(body)
      end
    end
  end

  defmacrop unregistered?(id, do: body, else: other) do
    quote do
      var!(user) = call { :user, :get, unquote(id) }

      if var!(user) |> User.registered? do
        unquote(other)
      else
        unquote(body)
      end
    end
  end

  start do
    { :ok, @state }
  end

  config _body do
    []
  end

  call { :config, name }, %{config: config} = state do
    { :ok, Dict.get(config, name), state }
  end

  call { :user, :new, client }, %{users: users} = state do
    %Connection.Client{host: host, port: port, secure: secure} = Idlate.connection(client, :details).client

    user  = %User{id: client, host: host, port: port, secure?: secure}
    state = %{state | users: users |> Dict.put(client, user)}

    { :ok, state }
  end

  call { :user, :get, nick }, %{users: users, nicks: nicks} = state when nick |> is_binary do
    if id = nicks |> Dict.get(nick |> String.downcase) do
      { :ok, users |> Dict.get(id), state }
    else
      { :ok, nil, state }
    end
  end

  call { :user, :get, id }, %{users: users} = state when id |> is_reference do
    { :ok, users |> Dict.get(id), state }
  end

  call { :user, :update, id, map }, %{users: users} = state do
    user = users |> Dict.get(id)
    user = user |> Map.merge(map)

    state = %{state | users: users |> Dict.put(id, user)}

    { :ok, user, state }
  end

  call { :user, :nick, id, nick }, %{users: users, nicks: nicks} = state do
    user = users |> Dict.get(id)
    old  = user.nick && String.downcase(user.nick)
    new  = String.downcase(nick)

    cond do
      old && old == new ->
        { :error, :current, state }

      nicks |> Dict.has_key?(new) ->
        { :error, :in_use, state }

      true ->
        if old do
          state = %{state | nicks: nicks |> Dict.delete(old)}
        end

        state = %{state | nicks: nicks |> Dict.put(new, id)}
        state = %{state | users: users |> Dict.put(id, %User{user | nick: nick})}

        { :ok, state }
    end
  end

  call { :user, :delete, id }, %{users: users, nicks: nicks, channels: channels} = state do
    if user = users |> Dict.get(id) do
      users = users |> Dict.delete(id)

      if user.nick do
        nicks = nicks |> Dict.delete(user.nick |> String.downcase)
      end

      channels = channels
        |> Seq.map(&{ &1, %Channel{&2 | users: &2.users |> Dict.delete(id)} })
        |> Seq.into(%{})

      state = %{state | users: users, nicks: nicks, channels: channels}
    end

    { :ok, state }
  end

  call { :channel, :create, name }, %{channels: channels} = state do
    channel = %Channel{name: name}
    state   = %{state | channels: channels |> Dict.put(name, channel)}

    { :ok, channel, state }
  end

  call { :channel, :get, name }, %{channels: channels} = state do
    { :ok, channels |> Dict.get(name), state }
  end

  call { :channel, :join, name, id }, %{channels: channels} = state do
    channel = channels |> Dict.get(name)
    channel = %Channel{channel | users: channel.users |> Dict.put(id, %Channel.User{id: id})}
    state   = %{state | channels: channels |> Dict.put(name, channel)}

    { :ok, channel, state }
  end

  handle :connected, client do
    call { :user, :new, client }

    nil
  end

  handle :disconnected, client do
    call { :user, :delete, client }

    nil
  end

  decode "PASS " <> password, _ do
    %Event.Password{content: password |> String.strip}
  end

  handle %Event.Password{content: password}, client do
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

  decode "NICK " <> rest, _ do
    %Event.Nick{name: rest |> String.strip}
  end

  handle %Event.Nick{name: name}, client do
    case call { :user, :nick, client, name } do
      { :error, :in_use } ->
        %Error.NicknameInUse{name: name}

      { :error, :current } ->
        nil

      :ok ->
        nil
    end
  end

  decode "USER " <> rest, _ do
    [rest, real_name] = rest |> String.split(":", global: false)
    [name, modes, _]  = rest |> String.strip |> String.split(" ")

    %Event.User{name: name, real_name: real_name, modes: modes}
  end

  handle %Event.User{name: name, real_name: real_name}, client do
    unregistered? client do
      user = call { :user, :update, client, %{name: name, real_name: real_name} }

      if user.nick do
        [ %Response.Welcome{server: Idlate.name, mask: to_string(user)},
          %Response.HostedBy{server: Idlate.name, ip: "0.0.0.0", port: user.port, version: @version},
          %Response.ServCreatedOn{created_on: "last thursday"},
          %Response.ServInfo{host: Idlate.name, version: @version, user: "NZo", channel: "abcCehiIkKlLmnNoQsStuvVxyz"} ]
      end
    end
  end

  decode "PING " <> rest, _ do
    %Event.Ping{cookie: rest}
  end

  handle %Event.Ping{cookie: cookie}, client do
    registered? client do
      %Event.Pong{cookie: cookie}
    end
  end

  encode %Event.Pong{cookie: cookie}, _ do
    "PONG #{cookie}"
  end

  decode "JOIN 0", _ do
    %Event.Part{reason: "Left all channels"}
  end

  decode "JOIN " <> rest, _ do
    Seq.map String.split(rest, ","), fn rest ->
      case rest |> String.strip |> String.split(" ") do
        [channel, password] ->
          %Event.Join{channel: channel |> String.rstrip, password: password |> String.lstrip}

        [channel] ->
          %Event.Join{channel: channel}
      end
    end
  end

  # TODO: check password
  handle %Event.Join{channel: name, password: _password}, client do
    registered? client do
      unless call { :channel, :get, name } do
        call { :channel, :create, name }
      end

      call { :channel, :join, name, client }
    end
  end

  decode "PRIVMSG " <> rest, _ do
    [to, content] = String.split(rest, ":", global: false)

    recipient = case to |> String.rstrip do
      << type :: utf8, _ :: binary >> = name when type in [?&, ?#, ?+, ?!] ->
        { :channel, name }

      name ->
        { :user, name }
    end

    %Event.Message{to: recipient, content: content}
  end

  handle %Event.Message{to: { :channel, name }, content: content}, client do
    registered? client do
      if channel = call { :channel, :get, name } do
        channel.users |> Dict.keys
          |> Seq.map(&{ &1, %Event.Message{from: user, to: { :channel, name }, content: content} })
          |> Dict.delete(client)
      else
        %Error.NoSuchChannel{channel: name}
      end
    end
  end

  handle %Event.Message{to: { :user, name }, content: content}, client do
    registered? client do
      if to = call { :user, :get, name } do
        { to.id, %Event.Message{from: user, to: { :user, name }, content: content} }
      else
        %Error.NoSuchNick{nick: name}
      end
    end
  end

  encode %Event.Message{from: from, to: { _, to }, content: content}, _client do
    ":#{from} PRIVMSG #{to} :#{content}"
  end

  require Response

  Seq.each Response.names, fn name ->
    encode %Response.unquote(name){} = value, client do
      Numeric.to_string(Idlate.name, call({ :user, :get, client }).nick, value)
    end
  end

  require Error

  Seq.each Error.names, fn name ->
    encode %Error.unquote(name){} = value, client do
      Numeric.to_string(Idlate.name, call({ :user, :get, client }).nick, value)
    end
  end
end
