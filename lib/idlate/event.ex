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
    case plugins |> Enum.find_value &(&1.input(line)) do
      nil ->
        do_trigger(plugins, client, { :unhandled, client, line }, false)

      event ->
        do_trigger(plugins, client, event.update(client: client), false)
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
    event = Enum.reduce plugins, plugin.pre(event) || event, fn plugin, event ->
      case plugin.pre(event) do
        nil ->
          event

        event ->
          event
      end
    end

    { _, event } = Enum.reduce plugins, { event, plugin.handle(event) }, fn
      plugin, { event, nil } ->
        { event, plugin.handle(event) }

      _plugin, { event, result } ->
        { event, result }
    end

    if event do
      event = Enum.reduce plugins, plugin.post(event) || event, fn plugin, event ->
        case plugin.post(event) do
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

  def reply(client, plugins, event) do
    case output(plugins, event) do
      nil ->
        nil

      { clients, output } ->
        Enum.each clients, &Idlate.Client.send(&1, output)

      output when output |> is_binary ->
        Idlate.Client.send(client, output)

      outputs when outputs |> is_list ->
        Enum.each outputs, fn
          { client, output } ->
            Idlate.Client.send(client, output)

          output ->
            Idlate.Client.send(client, output)
        end
    end
  end

  defp output(plugins, { clients, output }) when output |> is_list do
    output = Enum.map output, fn output ->
      Enum.find_value plugins, &(&1.output(output))
    end

    { clients, output }
  end

  defp output(plugins, output) when output |> is_list do
    Enum.map output, fn
      { client, output } ->
        { client, Enum.find_value(plugins, &(&1.output(output))) }

      output ->
        Enum.find_value plugins, &(&1.output(output))
    end
  end

  defp output(plugins, { clients, output }) do
    { clients, Enum.find_value(plugins, &(&1.output(output))) }
  end

  defp output(plugins, output) do
    Enum.find_value plugins, &(&1.output(output))
  end
end
