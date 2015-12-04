-module(tempfile_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  random:seed(erlang:system_time(micro_seconds)),
  ok.

stop(_State) ->
  ok.