server do
  name "idlate"

  listen 6667
end

plugin RFC281X do
  motd "We like trains, and tanks."

  timeout 60

  messages do
    part "%{message}"
    quit "Quit: %{message}"
    kill "Killed by %{sender.nick}: %{message}"
  end
end
