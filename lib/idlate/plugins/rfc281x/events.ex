use Idlate.RFC281X.DSL

defevent Password, content: nil
defevent Nick, name: nil
defevent User, name: nil, real_name: nil, modes: nil

defevent Ping, cookie: nil
defevent Pong, cookie: nil

defevent Join, channel: nil, password: nil
defevent Part, channel: nil, reason: nil
defevent Message, from: nil, to: nil, content: nil
