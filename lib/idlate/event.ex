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
    Process.spawn __MODULE__, :do_trigger, [client, event, true]
  end

  def trigger(plugins, client, event) do
    Process.spawn __MODULE__, :do_trigger, [plugins, client, event, true]
  end

  def do_parse(client, line) do
    do_parse(Idlate.plugins, client, line)
  end

  def do_parse(plugins, client, line) do
    case plugins |> Enum.find_value &(&1.input(line).update(client: client)) do
      nil ->
        do_trigger(plugins, client, { :unhandled, line }, false)

      event ->
        do_trigger(plugins, client, event, false)
    end
  end

  def do_trigger(client, event, custom) do
    do_trigger(Idlate.plugins, client, event, custom)
  end

  def do_trigger(plugins, client, event, custom) do
    output = case plugins do
      [plugin] ->
        event = case event |> plugin.pre do
          nil ->
            event

          event ->
            event
        end

        case event |> plugin.handle do
          nil ->
            nil

          event ->
            event |> plugin.output
        end

      [plugin | plugins] ->
        event = Enum.reduce plugins, event |> plugin.pre, fn plugin, event ->
          case plugin.pre(event) do
            nil ->
              event

            event ->
              event
          end
        end

        { _, event } = Enum.reduce plugins, { event, event |> plugin.handle }, fn
          plugin, { event, nil } ->
            { event, plugin.handle(event) }

          _plugin, { event, result } ->
            { event, result }
        end

        if event do
          event = Enum.reduce plugins, event |> plugin.post, fn plugin, event ->
            case plugin.post(event) do
              nil ->
                event

              event ->
                event
            end
          end
        end

        event
    end

    if output do
      case plugins |> Enum.find_value &(&1.output(output)) do
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

    unless custom do
      Idlate.Client.handled(client)
    end
  end
end
