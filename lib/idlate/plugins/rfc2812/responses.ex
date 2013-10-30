use Idlate.RFC2812

defnumeric Response.Welcome, 1, [:server, :mask] do
  def to_string(__MODULE__[server: server, mask: mask]) do
    ":Welcome to the #{server} #{mask}"
  end
end
