% @doc
% tempfile should have been started before using functions that generate
% random strings (the ones using temp_utils:randstr/1), as initialization
% reset the random generator seed, in order to guarantee a good distribution
% of random strings.
% @end

-module(tempfile_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  _ = random:seed(erlang:system_time(micro_seconds)),
  ok.

stop(_State) ->
  ok.
