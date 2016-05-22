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

defmodule Idlate.RFC281X.User do
  alias __MODULE__, as: T

  defstruct id:        nil,
            host:      nil,
            port:      nil,
            secure?:   false,
            nick:      nil,
            name:      nil,
            real_name: nil,
            modes:     MapSet.new,
            channels:  MapSet.new

  def registered?(%T{nick: nick, name: name}) when nick |> is_nil or name |> is_nil do
    false
  end

  def registered?(_) do
    true
  end

  defimpl String.Chars do
    def to_string(%T{nick: nick, name: name, host: host}) do
      "#{nick}!#{name}@#{host}"
    end
  end
end
