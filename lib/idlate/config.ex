defmodule Idlate.Config do
  def load(path) do
    Code.compile_string "import Idlate.Config; #{File.read!(path)}"
  end

  defmacro server(do: { :__block__, _, body }) do
    Enum.each body, fn
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
