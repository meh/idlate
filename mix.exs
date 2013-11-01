defmodule Idlate.Mixfile do
  use Mix.Project

  def project do
    [ app: :idlate,
      version: "0.0.1",
      elixir: "~> 0.10.4-dev",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:socket],
      mod: { Idlate, [config: "config.exs"] } ]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "~> 0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    [ { :reagent, github: "meh/reagent" },
      { :continuum, github: "meh/continuum" } ]
  end
end
