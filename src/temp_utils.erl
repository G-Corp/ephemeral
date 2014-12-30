%% @hidden
-module(temp_utils).
-define(CHARS, "azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN1234567890").

-export([
         randstr/1
        ]).

randstr(Size) ->
  [lists:sublist(?CHARS, random:uniform(length(?CHARS)), 1) || _ <- lists:seq(1, Size)].

