defmodule Idlate.Event do
  use Data
  import Kernel, except: [send: 2]

  @doc """
  Parses an event from a binary and triggers it.
  """
  def parse(client, input, at \\ :erlang.timestamp, plugins \\ Idlate.plugins) do
    spawn __MODULE__, :do_parse, [client, input, at, plugins]
  end

  @doc """
  Triggers an event asynchronously.
  """
  def trigger(client, event, at \\ :erlang.timestamp, plugins \\ Idlate.plugins) do
    spawn __MODULE__, :do_trigger, [client, event, at, plugins]
  end

  @doc """
  Triggers an event synchronously.
  """
  def trigger!(client, event, at \\ :erlang.timestamp, plugins \\ Idlate.plugins) do
    do_trigger(client, event, at, plugins)
  end

  @doc """
  Unroll nested events into a flat list.
  """
  def unroll(nil) do
    []
  end

  def unroll(event) when event |> is_list do
    Seq.flat_map event, &unroll(&1)
  end

  def unroll({ recipient, event }) when recipient |> is_reference do
    Seq.map unroll(event), &{ recipient, &1 }
  end

  def unroll(event) do
    [event]
  end

  def do_parse(client, input, at, plugins) do
    input = Seq.reduce plugins, input, fn plugin, input ->
      case plugin.input(input, client, at) do
        nil ->
          input

        input ->
          input
      end
    end

    case plugins |> Seq.find_value(&(&1.decode(input, client, at))) do
      nil ->
        do_trigger(client, { :unknown, input }, at, plugins)

      event ->
        do_trigger(client, event, at, plugins)
    end
  end

  def do_trigger(client, event, at, plugins) do
    event = Seq.reduce plugins, event, fn plugin, event ->
      Seq.flat_map unroll(event), fn event ->
        case plugin.pre(event, client, at) do
          nil ->
            event

          event ->
            event
        end
      end
    end

    event = Seq.flat_map unroll(event), fn event ->
      { _, event } = Seq.reduce plugins, { event, nil }, fn
        plugin, { event, nil } ->
          { event, plugin.handle(event, client, at) }

        _plugin, { event, result } ->
          { event, result }
      end

      event
    end

    unless Data.empty?(event) do
      event = Seq.reduce plugins, event, fn plugin, event ->
        Seq.flat_map unroll(event), fn event ->
          case plugin.post(event, client, at) do
            nil ->
              event

            event ->
              event
          end
        end
      end
    end

    unless Data.empty?(event) do
      reply(client, event, at, plugins)
    end
  end

  @doc """
  Sends a reply to the given client.
  """
  def reply(id, output, at \\ :erlang.timestamp, plugins \\ Idlate.plugins) do
    Seq.each unroll(output), &do_reply(id, &1, at, plugins)
  end

  defp do_reply(_id, { recipient, output }, at, plugins) when recipient |> is_reference do
    do_send(recipient, plugins |> Seq.find_value(&(&1.encode(output, recipient, at))), at, plugins)
  end

  defp do_reply(id, output, at, plugins) do
    do_send(id, plugins |> Seq.find_value(&(&1.encode(output, id, at))), at, plugins)
  end

  defp do_send(_id, nil, _at, _plugins), do: nil
  defp do_send(id, output, at, plugins) do
    output = Seq.reduce plugins, output, fn plugin, output ->
      case plugin.output(output, id, at) do
        nil ->
          output

        output ->
          output
      end
    end

    Socket.Stream.send!(Idlate.connection(id, :stream), [output, "\r\n"])
  end
end
