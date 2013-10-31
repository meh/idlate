use Idlate.RFC281X.DSL


# Reply format used by USERHOST to list replies to the query list. The reply string is composed as follows:
# <reply> ::= <nick>['*'] '=' <'+'|'-'><hostname>
# The '*' indicates whether the client has registered as an Operator.
# The '-' or '+' characters represent whether the client has set an AWAY message or not respectively.

defnumeric Response.UserHost, 302, [:nick, :operator?, :away?, :user, :host] do
  def to_string(__MODULE__[nick: nil]) do
    ":"
  end

  def to_string(__MODULE__[nick: nick, operator?: operator?, away?: away?, user: user, host: host]) do
    ":#{nick}=#{operator(operator?)} = #{away(away?)}#{user}@#{host}"
  end

  defp operator(true),  do: "*"
  defp operator(false), do: ""

  defp away(true),  do: "-"
  defp away(false), do: "+"
end

defnumeric Response.IsOn, 303, [:nicks] do
  def to_string(__MODULE__[nicks: nicks]) do
    ":#{nicks |> Enum.join(" ")}"
  end
end

defnumeric Response.Away, 301, [:nick, :away] do
  def to_string(__MODULE__[nick: nick, away: away]) do
    "#{nick} :#{away}"
  end
end

defnumeric Response.Unaway, 305, [] do
  def to_string() do
    ":You are no longer marked as being away"
  end
end

# These replies are used with the AWAY command (if allowed).
# RPL_AWAY is sent to any client sending a PRIVMSG to a client which is away.
# RPL_AWAY is only sent by the server to which the client is connected.
# Replies RPL_UNAWAY and RPL_NOWAWAY are sent when the client removes and sets an AWAY message.
defnumeric Response.NowAway, 306, [] do
  def to_string() do
    ":You have been marked as being away"
  end
end

defnumeric Response.WhoisUser, 311, [:nick, :user, :host, :real_name] do
  def to_string(__MODULE__[nick: nick, user: user, host: host, real_name: real_name]) do
    "#{nick} #{user} #{host} * :#{real_name}"
  end
end

defnumeric Response.WhoisMode, 379, [:nick, :modes] do
  def to_string(__MODULE__[nick: nick, modes: modes]) do
   "#{nick} :is using modes #{modes}"
  end
end

defnumeric Response.WhoisConnecting, 378, [:nick, :hostname, :ip] do
  def to_string(__MODULE__[nick: nick, hostname: hostname, ip: ip]) do
	"#{nick} :is connecting from *@#{hostname} #{ip}"
  end
end

defnumeric Response.WhoisServer, 312, [:nick, :host, :name] do
  def to_string(__MODULE__[nick: nick, host: host, name: name]) do
    "#{nick} #{host} :#{name}"
  end
end

defnumeric Response.WhoisOperator, 313, [:nick, :message] do
  def to_string(__MODULE__[nick: nick, message: message]) do
    "#{nick} :#{message}"
  end
end

defnumeric Response.WhoisIdle, 317, [:nick, :last_action_on, :connected_on] do
  def to_string(__MODULE__[nick: nick, last_action_on: last_action_on, connected_on: connected_on]) do
    now            = DateTime.now |> DateTime.to_epoch
    last_action_on = last_action_on |> DateTime.to_epoch
    connected_on   = connected_on |> DateTime.to_epoch

    "#{nick} #{now - last_action_on} #{connected_on} :seconds idle, signon time"
  end
end


defnumeric Response.EndOfWhois, 318, [:nick] do
  def to_string(__MODULE__[nick: nick]) do
    "#{nick} :End of /WHOIS list"
  end
end

# Replies 311 - 313, 317 - 319 are all replies generated in response to a WHOIS message.
# Given that there are enough parameters present, the answering server must either formulate a reply out of the above numerics (if the query nick is found) or return an error reply.
# The '*' in RPL_WHOISUSER is there as the literal character and not as a wild card.
# For each reply set, only RPL_WHOISCHANNELS may appear more than once (for long lists of channel names).
# The '@' and '+' characters next to the channel name indicate whether a client is a channel operator or has been granted permission to speak on a moderated channel.
# The RPL_ENDOFWHOIS reply is used to mark the end of processing a WHOIS message.
defnumeric Response.WhoisChannels, 319, [:nick, :channels] do
  def to_string(__MODULE__[nick: nick, channels: channels]) do
    "#{nick} :#{channels |> Enum.join(" ")}"
  end
end

defnumeric Response.WhoWasUser, 314, [:nick, :user, :host, :real_name] do
  def to_string(__MODULE__[nick: nick, user: user, host: host, real_name: real_name]) do
    "#{nick} #{user} #{host} * :#{real_name}"
  end
end

# When replying to a WHOWAS message, a server must use the replies RPL_WHOWASUSER, RPL_WHOISSERVER or ERR_WASNOSUCHNICK for each nickname in the presented list.
# At the end of all reply batches, there must be RPL_ENDOFWHOWAS (even if there was only one reply and it was an error).
defnumeric Response.EndOfWhowas, 369, [:nick] do
  def to_string(__MODULE__[nick: nick]) do
	"#{nick} :End of WHOWAS"
  end
end

defnumeric Response.ListStart, 321, [] do
  def to_string() do
    "Channel :Users Name"
  end
end

defnumeric Response.List, 322, [:name, :users, :modes, :topic] do
  def to_string(__MODULE__[name: name, users: users, modes: modes, topic: topic]) do
    "#{name} #{Enum.join(users, " ")} #{modes}:#{topic}"
  end
end

# Replies RPL_LISTSTART, RPL_LIST, RPL_LISTEND mark the start, actual replies with data and end of the server's response to a LIST command.
# If there are no channels available to return, only the start and end reply must be sent.
defnumeric Response.ListEnd, 323, [] do
  def to_string() do
    ":End of /LIST"
  end
end

defnumeric Response.ChannelModeIs, 324, [:name, :modes] do
  def to_string(__MODULE__[name: name, modes: modes]) do
    "#{name} #{modes}"
  end
end

defnumeric Response.NoTopic, 331, [:channel] do
  def to_string(__MODULE__[channel: channel]) do
    "#{channel} :No topic is set"
  end
end

# When sending a TOPIC message to determine the channel topic, one of two replies is sent.
# If the topic is set, RPL_TOPIC is sent back else RPL_NOTOPIC.
defnumeric Response.Topic, 332, [:channel, :topic] do
  def to_string(__MODULE__[channel: channel, topic: topic]) do
    "#{channel} :#{topic}"
  end
end

# Returned by the server to indicate that the attempted INVITE message was successful and is being passed onto the end client.
defnumeric Response.Inviting, 341, [:nick, :channel] do
  def to_string(__MODULE__[nick: nick, channel: channel]) do
    "#{nick} #{channel}"
  end
end

# Returned by a server answering a SUMMON message to indicate that it is summoning that user.
defnumeric Response.Summoing, 342, [:user] do
  def to_string(__MODULE__[user: user]) do
    "#{user} :Summoning user to IRC"
  end
end

# Reply by the server showing its version details.
# The <version> is the version of the software being used (including any patchlevel revisions) and the <debuglevel> is used to indicate if the server is running in "debug mode".
# The "comments" field may contain any comments about the version or further version details.
defnumeric Response.Version, 351, [:version, :host, :comments] do
  def to_string(__MODULE__[version: version, host: host, comments: comments]) do
    "idlate-#{version}. #{host} :#{comments}"
  end
end

defnumeric Response.WhoReply, 352, [:channel, :user, :host, :server, :nick, :away?, :ircop?, :level!, :hops, :real_name] do
  def to_string(__MODULE__[channel: channel, user: user, host: host, server: server, nick: nick, away?: away?, ircop?: ircop?, level!: level!, hops: hops, real_name: real_name]) do
    "#{channel} #{user} #{host} #{server} #{nick} #{away(away?)}#{ircop(ircop?)}#{level(level!)} :#{hops} #{real_name}"
  end

  defp away(true),  do: "G"
  defp away(false), do: "H"

  defp ircop(true),  do: "*"
  defp ircop(false), do: ""

  defp level(:voice),  do: "+"
  defp level(:chanop), do: "@"
  defp level(_),       do: ""
end

# The RPL_WHOREPLY and RPL_ENDOFWHO pair are used to answer a WHO message.
# The RPL_WHOREPLY is only sent if there is an appropriate match to the WHO query.
# If there is a list of parameters supplied with a WHO message, a RPL_ENDOFWHO must be sent after processing each list item with <name> being the item.
defnumeric Response.EndOfWho, 315, [:name] do
  def to_string(__MODULE__[name: name]) do
    "#{name} :End of /WHO list"
  end
end

defnumeric Response.NameReply, 353, [:channel, :users] do
  def to_string(__MODULE__[channel: channel, users: users]) do
    "= #{channel} :#{users |> Enum.join("")}"
  end
end

# To reply to a NAMES message, a reply pair consisting of RPL_NAMREPLY and RPL_ENDOFNAMES is sent by the server back to the client.
# If there is no channel found as in the query, then only RPL_ENDOFNAMES is returned.
# The exception to this is when a NAMES message is sent with no parameters and all visible channels and contents are sent back in a series of RPL_NAMEREPLY messages with a RPL_ENDOFNAMES to mark the end.
defnumeric Response.EndOfNames, 366, [:channel] do
  def to_string(__MODULE__[channel: channel]) do
    "#{channel} :End of /NAMES list"
  end
end

defnumeric Response.Links, 364, [:mask, :host, :hopcount, :info] do
  def to_string(__MODULE__[mask: mask, host: host, hopcount: hopcount, info: info]) do
    "#{mask} #{host} :#{hopcount} #{info}"
  end
end

# In replying to the LINKS message, a server must send replies back using the RPL_LINKS numeric and mark the end of the list using an RPL_ENDOFLINKS reply.v 
defnumeric Response.EndOfLinks, 365, [:mask] do
  def to_string(__MODULE__[mask: mask]) do
    "#{mask} :End of /LINKS list"
  end
end

defnumeric Response.BanList, 367, [:channel, :mask] do
  def to_string(__MODULE__[channel: channel, mask: mask]) do
    "#{channel} #{mask}"
  end
end

# When listing the active 'bans' for a given channel, a server is required to send the list back using the RPL_BANLIST and RPL_ENDOFBANLIST messages.
# A separate RPL_BANLIST is sent for each active banid. After the banids have been listed (or if none present) a RPL_ENDOFBANLIST must be sent.
defnumeric Response.EndOfBanList, 368, [:channel] do
  def to_string(__MODULE__[channel: channel]) do
    "#{channel} :End of channel ban list"
  end
end

defnumeric Response.ExceptionList, 348, [:channel, :mask] do
  def to_string(__MODULE__[channel: channel, mask: mask]) do
    "#{channel} #{mask}"
  end
end

defnumeric Response.EndOfExceptionList, 349, [:channel] do
  def to_string(__MODULE__[channel: channel]) do
    "#{channel} :End of channel exception list"
  end
end

defnumeric Response.InviteList, 346, [:channel, :mask] do
  def to_string(__MODULE__[channel: channel, mask: mask]) do
    "#{channel} #{mask}"
  end
end

defnumeric Response.EndOfInviteList, 347, [:channel] do
  def to_string(__MODULE__[channel: channel]) do
    "#{channel} :End of channel invite list"
  end
end

defnumeric Response.Info, 371, [:string] do
  def to_string(__MODULE__[string: string]) do
    ":#{string}"
  end
end

# A server responding to an INFO message is required to send all its 'info' in a series of RPL_INFO messages with a RPL_ENDOFINFO reply to indicate the end of the replies.
defnumeric Response.EndOfInfo, 374, [] do
  def to_string() do
    ":End of /INFO list"
  end
end

defnumeric Response.ModdStart, 375, [:host] do
  def to_string(__MODULE__[host: host]) do
    ":- #{host} Message of the day - "
  end
end

defnumeric Response.Motd, 372, [:text] do
  def to_string(__MODULE__[text: text]) do
    ":- #{text}"
  end
end

# When responding to the MOTD message and the MOTD file is found, the file is displayed line by line, with each line no longer than 80 characters, using RPL_MOTD format replies.
# These should be surrounded by a RPL_MOTDSTART (before the RPL_MOTDs) and an RPL_ENDOFMOTD (after).
defnumeric Response.EndOfMotd, 376, [] do
  def to_string() do
    ":End of /MOTD command"
  end
end

# RPL_YOUREOPER is sent back to a client which has just successfully issued an OPER message and gained operator status.
defnumeric Response.YourOper, 381, [] do
  def to_string() do
    ":You are now an IRC operator"
  end
end

# If the REHASH option is used and an operator sends a REHASH message, an RPL_REHASHING is sent back to the operator.
defnumeric Response.Rehashing, 382, [:path] do
  def to_string(__MODULE__[path: path]) do
    "#{path} :Rehashing"
  end
end

# When replying to the TIME message, a server must send the reply using the RPL_TIME format above.
# The string showing the time need only contain the correct day and time there.
# There is no further requirement for the time string.
defnumeric Response.Time, 391, [:host, :time] do
  def to_string(__MODULE__[host: host, time: time]) do
    "#{host} :#{time}"
  end
end

defnumeric Response.UsersStart, 392, [] do
  def to_string() do
    ":UserID Terminal Host"
  end
end

defnumeric Response.Users, 393, [:user, :tty, :host] do
  def to_string(__MODULE__[user: user, tty: tty, host: host]) do
    ":#{user} #{tty} #{host}"
  end
end

defnumeric Response.EndOfUsers, 394, [] do
  def to_string() do
    ":End of users"
  end
end

# If the USERS message is handled by a server, the replies RPL_USERSTART, RPL_USERS, RPL_ENDOFUSERS and RPL_NOUSERS are used.
# RPL_USERSSTART must be sent first, following by either a sequence of RPL_USERS or a single RPL_NOUSER.
# Following this is RPL_ENDOFUSERS.
defnumeric Response.NoUsers, 395, [] do
  def to_string() do
    ":Nobody logged in"
  end
end

defnumeric Response.TraceLink, 200, [:version, :debug_level, :dest, :next_host] do
  def to_string(__MODULE__[version: version, debug_level: debug_level, dest: dest, next_host: next_host]) do
    "Link #{version} #{debug_level} #{dest} #{next_host}"
  end
end

defnumeric Response.TraceConnecting, 201, [:class, :host] do
  def to_string(__MODULE__[class: class, host: host]) do
    "Try. #{class} #{host}"
  end
end

defnumeric Response.TraceHandshake, 202, [:class, :host] do
  def to_string(__MODULE__[class: class, host: host]) do
    "H.S. #{class} #{host}"
  end
end

defnumeric Response.TraceUnknown, 203, [:class, :ip] do
  def to_string(__MODULE__[class: class, ip: ip]) do
    "???? #{class} #{ip}"
  end
end

defnumeric Response.TraceOperator, 204, [:class, :nick] do
  def to_string(__MODULE__[class: class, nick: nick]) do
    "Oper #{class} #{nick}"
  end
end

defnumeric Response.Traceclass, 205, [:class, :nick] do
  def to_string(__MODULE__[class: class, nick: nick]) do
    "class #{class} #{nick}"
  end
end

defnumeric Response.TraceServer, 206, [:class] do
  def to_string(__MODULE__[class: class]) do
    "Serv #{class} <int>S <int>C <server> <nick!user|*!*>@<host|server>"
  end
end

defnumeric Response.Welcome, 1, [:server, :mask] do
  def to_string(__MODULE__[server: server, mask: mask]) do
    ":Welcome to the #{server} #{mask}"
  end
end

defnumeric Response.HostedBy, 2, [:bind, :ip, :port, :version] do
  def to_string(__MODULE__[bind: bind, ip: ip, port: port, version: version]) do
    ":Your host is #{bind}[#{ip}/#{port}], running version idlate-#{version}"
  end
end

defnumeric Response.ServCreatedOn, 3, [:created_on] do
  def to_string(__MODULE__[created_on: created_on]) do
	":This server was created #{created_on}"
  end
end

defnumeric Response.ServInfo, 4, [:host, :version, :client, :channel] do
  def to_string(__MODULE__[host: host, version: version, client: client, channel: channel]) do
    "#{host} idlate-#{version} #{client} #{channel}"
  end
end

defnumeric Resopnse.IsSupport, 5, [:value] do
  def to_string(__MODULE__[value: value]) do
	"#{value} :are supported by this server"
  end
end

defnumeric Response.ChanCreatedOn, 329, [:name, :created_on] do
  def to_string(__MODULE__[name: name, created_on: created_on]) do
    "#{name} #{created_on}"
  end
end

defnumeric Response.TopicSetOn, 333, [:channel_name, :set_by, :set_on] do
  def to_string(__MODULE__[channel_name: channel_name, set_by: set_by, set_on: set_on]) do
	"#{channel_name} #{set_by} #{set_on}"
  end
end

defnumeric Response.UsingSSL, 671, [:nick] do
  def to_string(__MODULE__[nick: nick]) do
	"#{nick} :is using a Secure Connection"
  end
end
