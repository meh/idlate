defmodule Idlate.RFC2812 do
  defmodule Numeric do
    def to_string(server, client, record) do
      ":#{server} #{pad((record |> elem(0)).number)} #{client} #{to_string(record)}"
    end

    def pad(n) when n < 10,   do: "00#{n}"
    def pad(n) when n < 100,  do: "0#{n}"
    def pad(n) when n < 1000, do: "#{n}"
  end

  defmacro __using__(_opts) do
    quote do
      import Idlate.RFC2812
    end
  end

  defmacro defnumeric(name, number, fields, do: body) do
    quote do
      defrecord Idlate.RFC2812.unquote(name), unquote(fields) do
        def number do
          unquote(number)
        end

        unquote(body)

        defimpl String.Chars, for: __MODULE__ do
          def to_string(self) do
            Idlate.RFC2812.unquote(name).to_string(self)
          end
        end
      end
    end
  end
end
