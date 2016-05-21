defmodule Idlate.Event do
  @moduledoc """
  input -> pre(*) -> handle -> post(*) -> output
  """

  import Kernel, except: [send: 2]

  def parse(client, line) do
    spawn __MODULE__, :do_parse, [client, line]
  end

  def parse(plugins, client, line) do
    spawn __MODULE__, :do_parse, [plugins, client, line]
  end

  def trigger(client, event) do
    spawn __MODULE__, :do_trigger, [client, event]
  end

  def trigger(plugins, client, event) do
    spawn __MODULE__, :do_trigger, [plugins, client, event]
  end

  def do_parse(client, line) do
    do_parse(Idlate.plugins, client, line)
  end

  def do_parse(plugins, client, line) do
    case plugins |> Enum.find_value(&(&1.input(line, client))) do
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
        reply(client, event, [plugin | plugins])
      end
    end
  end

  def reply(client, event) do
    reply(client, event, Idlate.plugins)
  end

  def reply(client, { recipient, output }, plugins) when output |> is_list do
    Enum.each output, &reply(client, { recipient, &1 }, plugins)
  end

  def reply(client, { recipient, output }, plugins) do
    Idlate.reply(recipient, output, plugins)
  end

  def reply(client, output, plugins) when output |> is_list do
    Enum.each output, &reply(client, &1, plugins)
  end

  def reply(client, output, plugins) do
    Idlate.reply(client, output, plugins)
  end
end
