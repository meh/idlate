defmodule Idlate.Plugin do
  defmacro __using__(_opts) do
    quote do
      alias Idlate.Event
      alias Idlate.Channel
      alias Idlate.Mask

      @behaviour Idlate.Plugin
      @before_compile unquote(__MODULE__)

      def init(_options) do
        { :ok, nil }
      end

      defoverridable init: 1

      def terminate(_reason, _state) do
        nil
      end

      defoverridable terminate: 2

      use GenServer.Behaviour

      def start_link(options) do
        :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, options, [])
      end

      def handle_call({ :call, args }, _from, state) do
        case call(args, state) do
          { :ok, reply, state } ->
            { :reply, reply, state }

          { :error, reason, state } ->
            { :reply, { :error, reason }, state }
        end
      end

      def handle_info(term, state) do
        case info(term, state) do
          { :ok, state } ->
            { :noreply, state }

          { :error, _reason, state } ->
            { :noreply, state }
        end
      end
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def handle(_) do
        :unimplemented
      end

      def event_for(_) do
        nil
      end

      def call(_, state) do
        { :error, :unimplemented, state }
      end

      def info(_, state) do
        { :ok, state }
      end
    end
  end
end
