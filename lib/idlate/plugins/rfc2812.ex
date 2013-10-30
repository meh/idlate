defmodule Idlate.RFC2812 do
  defmodule Numeric do
    def to_string(server, client, record) do
      ":#{server} #{pad((record |> elem(0)).number)} #{client} #{to_string(record)}"
    end

    def pad(n) when n < 10,   do: "00#{n}"
    def pad(n) when n < 100,  do: "0#{n}"
    def pad(n) when n < 1000, do: "#{n}"
  end

  use Idlate.Plugin
  alias __MODULE__.Event

  def event_for("PRIVMSG " <> rest) do
    [nick, content] = String.split(rest, ":", global: false)

    Event.Message[to: String.rstrip(nick), content: content]
  end

  def handle(Event.Message[]) do
    "hue"
  end
end
