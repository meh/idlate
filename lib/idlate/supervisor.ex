defmodule Idlate.Supervisor do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init(_) do
    children = [
      # Define workers and child supervisors to be supervised
      # worker(Idlate.Worker, [])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_one)
  end

  def listen(self, listener) do
    :supervisor.start_child self, worker(Reagent, [Idlate.Client, listener])
  end
end
