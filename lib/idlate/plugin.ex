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

defmodule Idlate.Plugin do
  use Behaviour

  @type line      :: binary
  @type in_event  :: term
  @type out_event :: term
  @type state     :: term
  @type client    :: reference
  @type at        :: :erlang.timestamp
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
  Function called by the event process with the raw input line, every plugin's
  `input` is called and the resulting binary is reduced on the rest, this means
  plugins can modify the incoming binary.
  """
  defcallback input(line, client, at) :: line

  @doc """
  Function called by the event process, only one plugin can return a parsed
  event, if no plugin knows about this message, an `{ :unknown, line }` event
  will be sent instead.
  """
  defcallback decode(line, client, at) :: in_event

  @doc """
  Function called by the event process with the parsed input event, every
  plugin's `pre` is called and the resulting input event is reduced on the
  rest, this means plugins can modify this input event.
  """
  defcallback pre(in_event, client, at) :: in_event

  @doc """
  Function called by the event process with the result of the `pre` step, only
  one plugin can return a resulting output event.
  """
  defcallback handle(in_event, client, at) :: out_event

  @doc """
  Function called by the event process with the parsed output event, every
  plugin's `post` is called and the resulting output event is reduced on the
  rest, this means plugins can modify this output event.
  """
  defcallback post(out_event, client, at) :: out_event

  @doc """
  Function called by the event process with the resulting output event from the
  `post` step, only one plugin can return a resulting output.
  """
  defcallback encode(out_event, client, at) :: line

  @doc """
  Function called by the event process with the raw output line, every plugin's
  `output` is called and the resulting binary is reduced on the rest, this
  means plugins can modify the outgoing binary.
  """
  defcallback output(line, client, at) :: line

  defmacro __using__(_opts) do
    quote do
      import          unquote(__MODULE__)
      @behaviour      unquote(__MODULE__)
      @before_compile unquote(__MODULE__)

      @priority 0
      @version "0.0.1"

      @input  false
      @decode false
      @pre    false
      @handle false
      @post   false
      @encode false
      @output false

      def start_link(options) do
        :gen_server.start_link({ :local, __MODULE__ }, __MODULE__, options, [])
      end

      def config(_) do
        []
      end

      defoverridable config: 1

      use GenServer

      def init(_options) do
        { :ok, nil }
      end

      defoverridable init: 1

      def terminate(_reason, _state) do
        nil
      end

      defoverridable terminate: 2

      def code_change(_, state, _) do
        { :ok, state }
      end

      defoverridable code_change: 3

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
      Enum.each [:input, :decode, :pre, :handle, :post, :encode, :output], fn name ->
        unless Module.get_attribute __MODULE__, name do
          def unquote(name)(_, _, _), do: nil
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

      def version do
        @version
      end
    end
  end

  @doc """
  Defines the plugin priority, events are dispatched in priority order.
  """
  defmacro priority(n) do
    quote do
      @priority unquote(n)
    end
  end

  @doc """
  Defines plugin initialization code.
  """
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

  @doc """
  Defines plugin deinitialization.
  """
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

  @doc """
  Defines configuration parameters.
  """
  defmacro config(variable, do: body) do
    quote do
      def config(unquote(variable)) do
        unquote(body)
      end
    end
  end

  @doc """
  Defines a plugin remote call.
  """
  defmacro call(args, { :when, _, [state, guard] }, do: body) do
    quote do
      def call(unquote(args), unquote(state)) when unquote(guard) do
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

  @doc """
  Handles custom messages.
  """
  defmacro info(args, state, do: body) do
    quote do
      def info(unquote(args), unquote(state)) do
        unquote(body)
      end
    end
  end

  @doc """
  Defines the plugin version.
  """
  defmacro version(name) do
    quote do
      @version unquote(name)
    end
  end

  @doc """
  Defines plugin update code.
  """
  defmacro version(old, state, do: body) do
    quote do
      def code_change(unquote(old), unquote(state), _) do
        unquote(body)
      end
    end
  end

  defmacro version(old, state, extra, do: body) do
    quote do
      def code_change(unquote(old), unquote(state), unquote(extra)) do
        unquote(body)
      end
    end
  end

  Enum.each [:input, :decode, :pre, :handle, :post, :encode, :output], fn name ->
    # One argument, with guard.
    defmacro unquote(name)({ :when, _, [match, guard] }, do: body) do
      name = unquote(name)

      quote do
        if Module.get_attribute __MODULE__, unquote(name) do
          raise ArgumentError, "catch all already defined"
        end

        def unquote(name)(unquote(match), _, _) when unquote(guard) do
          unquote(body)
        end
      end
    end

    # One argument, catch all.
    defmacro unquote(name)({ var, _, nil } = match, do: body) when var |> is_atom do
      name = unquote(name)

      quote do
        Module.put_attribute __MODULE__, unquote(name), true

        def unquote(name)(unquote(match), _, _) do
          unquote(body)
        end
      end
    end

    # One argument, with pattern match.
    defmacro unquote(name)(match, do: body) do
      name = unquote(name)

      quote do
        if Module.get_attribute __MODULE__, unquote(name) do
          raise ArgumentError, "catch all already defined"
        end

        def unquote(name)(unquote(match), _, _) do
          unquote(body)
        end
      end
    end

    # Two arguments, with guard.
    defmacro unquote(name)(match, { :when, _, [client, guard] }, do: body) do
      name = unquote(name)

      quote do
        if Module.get_attribute __MODULE__, unquote(name) do
          raise ArgumentError, "catch all already defined"
        end

        def unquote(name)(unquote(match), unquote(client), _) when unquote(guard) do
          unquote(body)
        end
      end
    end

    # Two arguments, catch all.
    defmacro unquote(name)({ var, _, nil } = match, client, do: body) when var |> is_atom do
      name = unquote(name)

      quote do
        Module.put_attribute __MODULE__, unquote(name), true

        def unquote(name)(unquote(match), unquote(client), _) do
          unquote(body)
        end
      end
    end

    # Two arguments, with pattern match.
    defmacro unquote(name)(match, client, do: body) do
      name = unquote(name)

      quote do
        if Module.get_attribute __MODULE__, unquote(name) do
          raise ArgumentError, "catch all already defined"
        end

        def unquote(name)(unquote(match), unquote(client), _) do
          unquote(body)
        end
      end
    end

    # Three arguments, with guard.
    defmacro unquote(name)(match, client, { :when, _, [at, guard] }, do: body) do
      name = unquote(name)

      quote do
        if Module.get_attribute __MODULE__, unquote(name) do
          raise ArgumentError, "catch all already defined"
        end

        def unquote(name)(unquote(match), unquote(client), unquote(at)) when unquote(guard) do
          unquote(body)
        end
      end
    end

    # Three arguments, catch all.
    defmacro unquote(name)({ var, _, nil } = match, client, at, do: body) when var |> is_atom do
      name = unquote(name)

      quote do
        Module.put_attribute __MODULE__, unquote(name), true

        def unquote(name)(unquote(match), unquote(client), unquote(at)) do
          unquote(body)
        end
      end
    end

    # Three arguments, with pattern match.
    defmacro unquote(name)(match, client, at, do: body) do
      name = unquote(name)

      quote do
        if Module.get_attribute __MODULE__, unquote(name) do
          raise ArgumentError, "catch all already defined"
        end

        def unquote(name)(unquote(match), unquote(client), unquote(at)) do
          unquote(body)
        end
      end
    end
  end
end
