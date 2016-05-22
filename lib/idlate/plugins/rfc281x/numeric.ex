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

defmodule Idlate.RFC281X.Numeric do
  def to_string(server, client, %{__struct__: module} = value) do
    ":#{server} #{pad(module.number)} #{client || "*"} #{module.to_string(value)}"
  end

  def pad(n) when n < 10,   do: "00#{n}"
  def pad(n) when n < 100,  do: "0#{n}"
  def pad(n) when n < 1000, do: "#{n}"
end
