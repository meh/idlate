defmodule Idlate.Event do
  @moduledoc """
  input -> pre(*) -> handle -> post(*) -> output
  """

  def parse(client, line) do
    Process.spawn __MODULE__, :do_parse, [client, line]
  end

  def parse(plugins, client, line) do
    Process.spawn __MODULE__, :do_parse, [plugins, client, line]
  end

  def trigger(client, event, custom // true) when custom in [true, false] do
    Process.spawn __MODULE__, :do_trigger, [client, event, custom]
  end

  def trigger(plugins, client, event) do
    trigger(plugins, client, event, true)
  end

  def trigger(plugins, client, event, custom) do
    Process.spawn __MODULE__, :do_trigger, [plugins, client, event, custom]
  end

  def do_parse(client, line) do
    do_parse(Idlate.plugins, client, line)
  end

  def do_parse(plugins, client, line) do
    case plugins |> Enum.find_value &(&1.input(line, client)) do
      nil ->
        do_trigger(plugins, client, { :unhandled, line }, false)

      event ->
        do_trigger(plugins, client, event, false)
    end
  end

  def do_trigger(client, event, custom) do
    do_trigger(Idlate.plugins, client, event, custom)
  end

  def do_trigger([], client, _, custom) do
    unless custom do
      Idlate.Client.handled(client)
    end
  end

  def do_trigger([plugin | plugins], client, event, custom) do
    event = Enum.reduce plugins, plugin.pre(event, client) || event, fn plugin, event ->
      case plugin.pre(event, client) do
        nil ->
          event

        event ->
          event
      end
    end

    { _, event } = Enum.reduce plugins, { event, plugin.handle(event, client) }, fn
      plugin, { event, nil } ->
        { event, plugin.handle(event, client) }

      _plugin, { event, result } ->
        { event, result }
    end

    if event do
      event = Enum.reduce plugins, plugin.post(event, client) || event, fn plugin, event ->
        case plugin.post(event, client) do
          nil ->
            event

          event ->
            event
        end
      end
    end

    if event do
      reply(client, [plugin | plugins], event)
    end

    unless custom do
      Idlate.Client.handled(client)
    end
  end

  def reply(client, event) do
    reply(client, Idlate.plugins, event)
  end

  def reply(client, plugins, { clients, outputs }) when not clients |> is_atom and outputs |> is_list do
    Enum.each outputs, &reply(client, plugins, { clients, &1 })
  end

  def reply(client, plugins, { clients, output }) when not clients |> is_atom do
    Enum.each List.wrap(clients), fn client ->
      client |> Idlate.Client.send Enum.find_value(plugins, &(&1.output(output, client)))
    end
  end

  def reply(client, plugins, outputs) when outputs |> is_list do
    Enum.each outputs, &reply(client, plugins, &1)
  end

  def reply(client, plugins, output) do
    client |> Idlate.Client.send Enum.find_value(plugins, &(&1.output(output, client)))
  end
end
