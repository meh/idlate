defmodule Idlate.RFC2812.DSL do
  defmacro __using__(_opts) do
    quote do
      import unquote(__MODULE__)
    end
  end

  defmacro defnumeric(name, number, fields, do: body) do
    quote do
      defrecord Idlate.RFC2812.unquote(name), unquote(fields) do
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
      defrecord Idlate.RFC2812.Event.unquote(name), unquote(fields)
    end
  end

  defmacro defevent(name, fields, do: body) do
    quote do
      defrecord Idlate.RFC2812.Event.unquote(name), unquote(fields) do
        unquote(body)
      end
    end
  end
end
