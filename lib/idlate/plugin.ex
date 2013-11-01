defmodule Idlate.Plugin do
  use Behaviour

  @type in_event  :: term
  @type out_event :: term
  @type state     :: term
  @type client    :: pid
  @type output    :: term

  @doc """
  Call to the plugin gen_server, mostly used for state stuff.

  Note the code is executed in the gen_server, so the process will block until
  the call is finished, if multiple events are calling this, they will have to
  wait.
  """
  defcallback call(term, state) :: { :ok, state } | { :error, term, state }

  @doc """
  Message sent to the plugin gen_server.
  """
  defcallback info(term, state) :: { :ok, state } | { :error, term, state }

  @doc """
  Function called by the event process, only one plugin can return a parsed
  event, if no plugin knows about this message, an `{ :unhandled, line }` event
  will be sent instead.
  """
  defcallback input(String.t) :: in_event

  @doc """
  Function called by the event process with the parsed input event, every
  plugin's `pre` is called and the resulting input event is reduced on the
  rest, this means plugins can modify this input event.
  """
  defcallback pre(in_event :: term) :: in_event

  @doc """
  Function called by the event process with the result of the `pre` step, only
  one plugin can return a resulting output event.
  """
  defcallback handle(in_event) :: out_event

  @doc """
  Function called by the event process with the parsed output event, every
  plugin's `post` is called and the resulting output event is reduced on the
  rest, this means plugins can modify this output event.
  """
  defcallback post(out_event) :: out_event

  @doc """
  Function called by the event process with the resulting output event from the
  `post` step, only one plugin can return a resulting output.
  """
  defcallback output(out_event) ::
    output | { [client], output | [output] } | [output | { client, output }]

  defmacro __using__(_opts) do
    quote do
      import          unquote(__MODULE__)
      @behaviour      unquote(__MODULE__)
      @before_compile unquote(__MODULE__)

      @priority 0

      @input  false
      @pre    false
      @handle false
      @post   false
      @output false

      def start_link(options) do
        :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, options, [])
      end

      def config(_) do
        []
      end

      defoverridable config: 1

      use GenServer.Behaviour

      def init(_options) do
        { :ok, nil }
      end

      defoverridable init: 1

      def terminate(_reason, _state) do
        nil
      end

      defoverridable terminate: 2

      def handle_call(args, _from, state) do
        case call(args, state) do
          { :ok, reply, state } ->
            { :reply, reply, state }

          { :ok, state } ->
            { :reply, :ok, state }

          { :error, reason, state } ->
            { :reply, { :error, reason }, state }

          { :error, state } ->
            { :reply, :error, state }
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
    quote unquote: false do
      Enum.each [:input, :pre, :handle, :post, :output], fn name ->
        unless Module.get_attribute __MODULE__, name do
          def unquote(name)(_), do: nil
        end
      end

      def call(_, state) do
        { :error, :unimplemented, state }
      end

      def info(_, state) do
        { :ok, state }
      end

      def priority do
        @priority
      end
    end
  end

  defmacro priority(n) do
    quote do
      @priority unquote(n)
    end
  end

  defmacro start(do: body) do
    quote do
      def init(_) do
        unquote(body)
      end
    end
  end

  defmacro start(args, do: body) do
    quote do
      def init(unquote(args)) do
        unquote(body)
      end
    end
  end

  defmacro stop(reason, do: body) do
    quote do
      def terminate(unquote(reason), _) do
        unquote(body)
      end
    end
  end

  defmacro stop(reason, state, do: body) do
    quote do
      def terminate(unquote(reason), unquote(state)) do
        unquote(body)
      end
    end
  end

  defmacro config(variable, do: body) do
    quote do
      def config(unquote(variable)) do
        unquote(body)
      end
    end
  end

  defmacro call(args, state, do: body) do
    quote do
      def call(unquote(args), unquote(state)) do
        unquote(body)
      end
    end
  end

  defmacro info(args, state, do: body) do
    quote do
      def info(unquote(args), unquote(state)) do
        unquote(body)
      end
    end
  end

  Enum.each [:input, :pre, :handle, :post, :output], fn name ->
    defmacro unquote(name)({ var, _, _ } = match, do: body) when var |> is_atom do
      name = unquote(name)

      quote do
        Module.put_attribute __MODULE__, unquote(name), true

        def unquote(name)(unquote(match)) do
          unquote(body)
        end
      end
    end

    defmacro unquote(name)(match, do: body) do
      name = unquote(name)

      quote do
        if Module.get_attribute __MODULE__, unquote(name) do
          raise ArgumentError, "catch all already defined"
        end

        def unquote(name)(unquote(match)) do
          unquote(body)
        end
      end
    end
  end
end
