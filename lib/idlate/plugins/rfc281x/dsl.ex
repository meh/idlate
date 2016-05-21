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

defmodule Idlate.RFC281X.DSL do
  defmacro __using__(_opts) do
    quote do
      import unquote(__MODULE__)

      alias Idlate.RFC281X.User
      alias Idlate.RFC281X.Channel
    end
  end

  defmacro defnumeric(name, do: body) do
    quote do
      defmodule Idlate.RFC281X.unquote(name) do
        use Idlate.RFC281X.DSL

        Module.register_attribute __MODULE__, :names, accumulate: true

        unquote(body)

        def names do
          @names
        end
      end
    end
  end

  defmacro defnumeric(name, number, fields \\ [], do: body) do
    quote do
      @names unquote(name)

      defmodule unquote(name) do
        defstruct unquote(fields)

        def number do
          unquote(number)
        end

        unquote(body)

        defimpl String.Chars do
          defdelegate to_string(self), to: @for
        end
      end
    end
  end

  defmacro defevent(name, fields) do
    quote do
      defmodule Idlate.RFC281X.Event.unquote(name) do
        defstruct unquote(fields)
      end
    end
  end

  defmacro defevent(name, fields, do: body) do
    quote do
      defmodule Idlate.RFC281X.Event.unquote(name) do
        defstruct unquote(fields)

        unquote(body)
      end
    end
  end
end
