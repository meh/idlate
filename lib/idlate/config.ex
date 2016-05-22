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

defmodule Idlate.Config do
  use Data

  def load(path) do
    Code.compile_string "import Idlate.Config; #{File.read!(path)}"
  end

  defmacro server(do: { :__block__, _, body }) do
    Seq.each body, fn
      { :name, _, [name] } ->
        :gen_server.cast Idlate, { :name, name }

      { :listen, _, [port] } ->
        :gen_server.cast Idlate, { :listen, [port: port] }
    end
  end

  defmacro plugin(name) do
    quote do
      :gen_server.cast Idlate, { :plugin, Idlate.unquote(name), [] }
    end
  end

  defmacro plugin(name, do: body) do
    quote do
      :gen_server.cast Idlate, { :plugin, Idlate.unquote(name),
        Idlate.unquote(name).config(unquote(Macro.escape(body))) }
    end
  end
end
