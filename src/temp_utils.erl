%% @hidden
-module(temp_utils).
-define(CHARS, "azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN1234567890").

-export([
         randstr/1,
         ensure_started/0
        ]).

randstr(Size) ->
  [lists:sublist(?CHARS, random:uniform(length(?CHARS)), 1) || _ <- lists:seq(1, Size)].

ensure_started() ->
  case [T || {A, _, _} = T <- application:which_applications(), A =:= tempfile] of
    [] -> application:ensure_all_started(tempfile);
    [{tempfile, _, _}|_] -> ok
  end.

