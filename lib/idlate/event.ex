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

  def trigger(client, event) do
    Process.spawn __MODULE__, :do_trigger, [client, event]
  end

  def trigger(plugins, client, event) do
    Process.spawn __MODULE__, :do_trigger, [plugins, client, event]
  end

  def do_parse(client, line) do
    do_parse(Idlate.plugins, client, line)
  end

  def do_parse(plugins, client, line) do
    case plugins |> Enum.find_value &(&1.input(line, client)) do
      nil ->
        do_trigger(plugins, client, { :unhandled, line })

      event ->
        do_trigger(plugins, client, event)
    end
  end

  def do_trigger(client, event) do
    do_trigger(Idlate.plugins, client, event)
  end

  def do_trigger([plugin | plugins], client, event) do
    Enum.each List.wrap(event), fn event ->
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
    end
  end

  def reply(client, event) do
    reply(client, Idlate.plugins, event)
  end

  def reply(client, plugins, { clients, outputs }) when not is_atom(clients) and is_list(outputs) do
    Enum.each outputs, &reply(client, plugins, { clients, &1 })
  end

  def reply(_client, plugins, { clients, output }) when not is_atom(clients) do
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
