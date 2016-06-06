defmodule Tempfile.Mixfile do
  use Mix.Project

  def project do
    [
      app: :tempfile,
      version: "1.0.2",
      elixir: "~> 1.2",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps
    ]
  end

  def application do
    [
       applications: [:kernel, :stdlib, :bucs],
       env: []
    ]
  end

  defp deps do
    [
      {:bucs, git: "https://github.com/botsunit/bucs.git", branch: "master"}    
    ]
  end
end