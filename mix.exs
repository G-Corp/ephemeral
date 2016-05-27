defmodule Tempfile.Mixfile do
  use Mix.Project

  def project do
    [app: :tempfile,
     version: "1.0.2",
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [applications: [:bucs]]
  end

  defp deps do
    [ 
      {:bucs, ~r/.*/, git: "https://github.com/botsunit/bucs.git", tag: "0.0.1"},
    ]
  end
end
