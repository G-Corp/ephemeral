-module(tempfile_tests).

-include_lib("eunit/include/eunit.hrl").

tempfile_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_name())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_name() ->
  DirPath  = tempfile:name("tutu", [{ext, ".toto"},{path, "tata"}]),
  ?assertMatch("tata", filename:dirname(DirPath)),
  ?assertMatch({match, _}, re:run(filename:basename(DirPath), "^tutu.{20}\\.toto$")).


