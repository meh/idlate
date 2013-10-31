defmodule Idlate.Plugin do
  use Behaviour

  @type in_event  :: term
  @type out_event :: term
  @type state     :: term

  defcallback call(term, state) :: { :ok, state } | { :error, term, state }
  defcallback info(term, state) :: { :ok, state } | { :error, term, state }

  defcallback input(String.t) :: in_event
  defcallback pre(in_event :: term) :: in_event
  defcallback handle(in_event) :: out_event
  defcallback post(out_event) :: out_event
  defcallback output(out_event) :: String.t

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

      def handle_call(args, _from, state) do
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

      def call(term) do
        :gen_server.call __MODULE__, term
      end
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      def input(_) do
        nil
      end

      def pre(_) do
        nil
      end

      def handle(_) do
        nil
      end

      def post(_) do
        nil
      end

      def output(_) do
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
