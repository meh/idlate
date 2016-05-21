# Copyleft (ɔ) meh. - http://meh.schizofreni.co
#
# This file is part of idlate - https://github.com/meh/idlate
#
# idlate is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# idlate is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with idlate. If not, see <http://www.gnu.org/licenses/>.

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
    List.wrap(clients) |> Enum.each(fn client ->
      send client, Enum.find_value(plugins, &(&1.output(output, client)))
    end)
  end

  def reply(client, plugins, outputs) when outputs |> is_list do
    Enum.each outputs, &reply(client, plugins, &1)
  end

  def reply(client, plugins, output) do
    send client, Enum.find_value(plugins, &(&1.output(output, client)))
  end

  defp send(client, data) when data |> is_list do
    Enum.each data, &Socket.Stream.send!(client, [&1, "\r\n"])
  end

  defp send(client, data) do
    Socket.Stream.send!(client, [data, "\r\n"])
  end
end
