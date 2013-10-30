use Idlate.RFC2812

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

