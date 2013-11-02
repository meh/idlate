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

  defmacro defnumeric(name, number, fields // [], do: body) do
    quote do
      @names unquote(name)

      defrecord unquote(name), [:client | unquote(fields)] do
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
      defrecord Idlate.RFC281X.Event.unquote(name), [:client | unquote(fields)]
    end
  end

  defmacro defevent(name, fields, do: body) do
    quote do
      defrecord Idlate.RFC281X.Event.unquote(name), [:client | unquote(fields)] do
        unquote(body)
      end
    end
  end
end
