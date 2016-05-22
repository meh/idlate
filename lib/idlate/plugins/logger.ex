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

defmodule Idlate.Logger do
  use Idlate.Plugin

  priority -10

  input string, client, at do
    IO.puts "#{timestamp(at)} <~ #{host_for(client)} #{string}"

    nil
  end

  pre event, client, at do
    IO.puts "#{timestamp(at)} <- #{host_for(client)} #{inspect event}"

    nil
  end

  post event, client, at do
    IO.puts "#{timestamp(at)} -> #{host_for(client)} #{inspect event}"

    nil
  end

  output string, client, at do
    IO.puts "#{timestamp(at)} ~> #{host_for(client)} #{string}"

    nil
  end

  defp timestamp(at) do
    { { year, month, day }, { hour, minute, second } }
      = :calendar.now_to_universal_time(at)

    :io_lib.format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [
      year, month, day, hour, minute, second])
  end

  defp host_for(client) do
    Idlate.connection(client, :details).client.host
  end
end
