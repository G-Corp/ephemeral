# File: Ephemeral.TempFile.ex
# This file was generated from tempfile.beam
# Using rebar3_elixir (https://github.com/G-Corp/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Ephemeral.TempFile do
  def unquote(:"name")(arg1) do
    :erlang.apply(:"tempfile", :"name", [arg1])
  end
  def unquote(:"name")(arg1, arg2) do
    :erlang.apply(:"tempfile", :"name", [arg1, arg2])
  end
end
