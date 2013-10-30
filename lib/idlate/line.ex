defmodule Idlate.Line do
  def start(server, client, line) do
    Process.spawn __MODULE__, :handle, [server, client, line]
  end

  def handle(server, client, line) do
    event = server |> Idlate.plugins |> Enum.find_value &(&1.event_for(line))
  end
end
