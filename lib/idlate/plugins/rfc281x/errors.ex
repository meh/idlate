use Idlate.RFC281X.DSL

defnumeric Error do
  # Used to indicate the nickname parameter supplied to a command is currently unused.
  defnumeric NoSuckNick, 401, [:nick] do
    def to_string(__MODULE__[nick: nick]) do
      "#{nick} :No such nick/channel"
    end
  end

  # Used to indicate the server name given currently doesn"t exist.
  defnumeric NoSuckServer, 402, [:server] do
    def to_string(__MODULE__[server: server]) do
      "#{server} :No such server"
    end
  end

  # Used to indicate the given channel name is invalid.
  defnumeric NoSuchChannel, 403, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :No such channel"
    end
  end

  # Sent to a user who is either (a) not on a channel which is mode +n or (b) not a chanop (or mode +v) on a channel which has mode +m set and is trying to send a PRIVMSG message to that channel.
  defnumeric CannotSendToChan, 404, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :Cannot send to channel"
    end
  end

  defnumeric YouNeedVoice, 404, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :You need voice (+v) (#{channel})"
    end
  end

  defnumeric NoExternalMessages, 404, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :No external channel messages (#{channel})"
    end
  end

  defnumeric YouAreBanned, 404, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :You are banned (#{channel})"
    end
  end

  defnumeric NoCTCPs, 404, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :CTCPs are not permitted in this channel (#{channel})"
    end
  end

  defnumeric NoColors, 404, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :Color is not permitted in this channel (#{channel})"
    end
  end

  # Sent to a user when they have joined the maximum number of allowed channels and they try to join another channel.
  defnumeric TooManyChannels, 405, [:name] do
    def to_string(__MODULE__[name: name]) do
      "#{name} :You have joined too many channels"
    end
  end

  # Sent to a user when they have joined the maximum number of allowed channels and they try to join another channel.
  defnumeric NoSuchNick, 406, [:nick] do
    def to_string(__MODULE__[nick: nick]) do
      "#{nick} :There was no such nickname"
    end
  end

  # Returned to a client which is attempting to send PRIVMSG/NOTICE using the user@host destination format and for a user@host which has several occurrences.
  defnumeric TooManyTargets, 407, [:recp] do
    def to_string(__MODULE__[recp: recp]) do
      "#{recp} :Duplicate recipients. No message delivered"
    end
  end

  # PING or PONG message missing the originator parameter which is required since these commands must work without valid prefixes.
  defnumeric NoOrigin, 409 do
    def to_string(_) do
      ":No origin specified"
    end
  end

  defnumeric NoRecipient, 411, [:recp] do
    def to_string(__MODULE__[recp: recp]) do
      ":No recipient given (#{recp})"
    end
  end

  defnumeric NoTextToSend, 412 do
    def to_string(_) do
      ":No text to send"
    end
  end

  defnumeric NoTopLevel, 413, [:mask] do
    def to_string(__MODULE__[mask: mask]) do
      "#{mask} :No toplevel domain specified"
    end
  end

  # 412 - 414 are returned by PRIVMSG to indicate that the message wasn"t delivered for some reason. ERR_NOTOPLEVEL and ERR_WILDTOPLEVEL are errors that are returned when an invalid use of "PRIVMSG $<server>" or "PRIVMSG #<host>" is attempted.
  defnumeric WildTopLevel, 414, [:mask] do
    def to_string(__MODULE__[mask: mask]) do
      "#{mask} :Wildcard in toplevel domain"
    end
  end

  # Returned to a registered client to indicate that the command sent is unknown by the server.
  defnumeric UnknownCommand, 421, [:command] do
    def to_string(__MODULE__[command: command]) do
      "#{command} :Unknown command"
    end
  end

  # Server"s MOTD file could not be opened by the server.
  defnumeric NoMOTD, 422 do
    def to_string(_) do
      ":MOTD File is missing"
    end
  end

  # Returned by a server in response to an ADMIN message when there is an error in finding the appropriate information.
  defnumeric NoAdminInfo, 423, [:server] do
    def to_string(__MODULE__[server: server]) do
      "#{server} :No administrative info available"
    end
  end

  # Generic error message used to report a failed file operation during the processing of a message.
  defnumeric FileError, 424, [:file_op, :file] do
    def to_string(__MODULE__[file_op: file_op, file: file]) do
      ":File error doing #{file_op} on #{file}"
    end
  end

  # Returned when a nickname parameter expected for a command and isn"t found.
  defnumeric NoNicknameGiven, 431 do
    def to_string(_) do
      ":No nickname given"
    end
  end

  # Returned after receiving a NICK message which contains characters which do not fall in the defined set. See section x.x.x for details on valid nicknames.
  defnumeric ErroneusNickname, 432, [:name] do
    def to_string(__MODULE__[name: name]) do
      "#{name} :Erroneus nickname"
    end
  end

  # Returned when a NICK message is processed that results in an attempt to change to a currently existing nickname.
  defnumeric NicknameInUse, 433, [:name] do
    def to_string(__MODULE__[name: name]) do
      "#{name} :Nickname is already in use"
    end
  end

  # Returned by a server to a client when it detects a nickname collision (registered of a NICK that already exists by another server).
  defnumeric NickCollision, 436, [:nick] do
    def to_string(__MODULE__[nick: nick]) do
      "#{nick} :Nickname collision KILL"
    end
  end

  # Returned by the server to indicate that the target user of the command is not on the given channel.
  defnumeric UserNotInChannel, 441, [:nick, :channel] do
    def to_string(__MODULE__[nick: nick, channel: channel]) do
      "#{nick} #{channel} :They aren\"t on that channel"
    end
  end

  # Returned by the server whenever a client tries to perform a channel effecting command for which the client isn"t a member.
  defnumeric NotOnChannel, 442, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :You\"re not on that channel"
    end
  end

  # Returned when a client tries to invite a user to a channel they are already on.
  defnumeric UserOnChannel, 443, [:nick, :channel] do
    def to_string(__MODULE__[nick: nick, channel: channel]) do
      "#{nick} #{channel} :is already on channel"
    end
  end

  # Returned by the summon after a SUMMON command for a user was unable to be performed since they were not logged in.
  defnumeric NoLogin, 444, [:user] do
    def to_string(__MODULE__[user: user]) do
      "#{user} :User not logged in"
    end
  end

  # Returned as a response to the SUMMON command. Must be returned by any server which does not implement it.
  defnumeric SummonDisabled, 445 do
    def to_string(_) do
      ":SUMMON has been disabled"
    end
  end

  # Returned as a response to the USERS command. Must be returned by any server which does not implement it.
  defnumeric UsersDisabled, 446 do
    def to_string(_) do
      ":USERS has been disabled"
    end
  end

  # Returned by the server to indicate that the client must be registered before the server will allow it to be parsed in detail.
  defnumeric NotRegistered, 451 do
    def to_string(_) do
      ":You have not registered"
    end
  end

  # Returned by the server by numerous commands to indicate to the client that it didn"t supply enough parameters.
  defnumeric NeedMoreParams, 461, [:command] do
    def to_string(__MODULE__[command: command]) do
      "#{command} :Not enough parameters"
    end
  end

  # Returned by the server to any link which tries to change part of the registered details (such as password or user details from second USER message).
  defnumeric AlreadyRegistered, 462 do
    def to_string(_) do
      ":You may not reregister"
    end
  end

  # Returned to a client which attempts to register with a server which does not been setup to allow connections from the host the attempted connection is tried.
  defnumeric NoPermForHost, 463 do
    def to_string(_) do
      ":Your host isn\"t among the privileged"
    end
  end

  # Returned to indicate a failed attempt at registering a connection for which a password was required and was either not given or incorrect.
  defnumeric PasswdMismatch, 464 do
    def to_string(_) do
      ":Password incorrect"
    end
  end

  # Returned after an attempt to connect and register yourself with a server which has been setup to explicitly deny connections to you.
  defnumeric YoureBannedCreep, 465 do
    def to_string(_) do
      ":You are banned from this server"
    end
  end

  defnumeric KeySet, 467, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :Channel key already set"
    end
  end

  defnumeric ChannelIsFull, 471, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :Cannot join channel (+l)"
    end
  end

  defnumeric UnknownMode, 472, [:mode] do
    def to_string(__MODULE__[mode: mode]) do
      "#{mode} :is unknown mode char to me"
    end
  end

  defnumeric InviteOnlyChan, 473, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :Cannot join channel (+i)"
    end
  end

  defnumeric BannedFromChan, 474, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :Cannot join channel (+b)"
    end
  end

  defnumeric BadChannelKey, 475, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :Cannot join channel (+k)"
    end
  end

  defnumeric NoKnock, 480, [:channel, :reason] do
    def to_string(__MODULE__[channel: channel, reason: reason]) do
      ":Cannot knock on #{channel} (#{reason})"
    end
  end

  # Any command requiring operator privileges to operate must return this error to indicate the attempt was unsuccessful.
  defnumeric NoPrivileges, 481 do
    def to_string(_) do
      ":Permission Denied- You\"re not an IRC operator"
    end
  end

  # Any command requiring "chanop" privileges (such as MODE messages) must return this error if the client making the attempt is not a chanop on the specified channel.
  defnumeric ChanOPrivsNeeded, 482, [:user] do
    def to_string(__MODULE__[user: user]) do
      "#{user} :You\"re not channel operator"
    end
  end

  # Any attempts to use the KILL command on a server are to be refused and this error returned directly to the client.
  defnumeric CantKillServer, 483 do
    def to_string(_) do
      ":You cant kill a server!"
    end
  end

  # If a client sends an OPER message and the server has not been configured to allow connections from the client"s host as an operator, this error must be returned.
  defnumeric NoOperHost, 491 do
    def to_string(_) do
      ":No O-lines for your host"
    end
  end

  # Returned by the server to indicate that a MODE message was sent with a nickname parameter and that the a mode flag sent was not recognized.
  defnumeric UmodeUnknownFlag, 501 do
    def to_string(_) do
      ":Unknown MODE flag"
    end
  end

  # Error SENT to any user trying to view or change the user mode for a user other than themselves.
  defnumeric UsersDontMatch, 502 do
    def to_string(_) do
      ":Cant change mode for other users"
    end
  end

  # custom
  defnumeric BadChanmask, 476, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :Bad channel name"
    end
  end

  defnumeric AllMustUseSSL, 974 do
    def to_string(_) do
      "z :all members must be connected via SSL"
    end
  end

  defnumeric SSLRequired, 489, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "#{channel} :Cannot join channel (SSL is required)"
    end
  end

  defnumeric NoNickChange, 447, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "Can not change nickname while on #{channel} (+N)"
    end
  end

  defnumeric NoKicks, 972 do
    def to_string(_) do
      "KICK :channel is +Q"
    end
  end

  defnumeric NoInvite, 518, [:channel] do
    def to_string(__MODULE__[channel: channel]) do
      "Cannot invite (+V) at channel #{channel}"
    end
  end
end
