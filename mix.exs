defmodule Idlate.Mixfile do
  use Mix.Project

  def project do
    [ app: :idlate,
      version: "0.0.1",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps,
      package: package,
      description: "Plugin based IRC server." ]
  end

  def application do
    [ applications: [:socket, :logger],
      mod: { Idlate, [config: "config/server.exs"] } ]
  end

  defp deps do
    [ { :reagent, github: "meh/reagent" },
      { :datastructures, "~> 0.2" } ]
  end

  defp package do
    [ maintainers: ["meh"],
      licenses: ["AGPLv3"],
      links: %{"GitHub" => "https://github.com/meh/idlate"} ]
  end
end
