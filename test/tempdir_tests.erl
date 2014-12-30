-module(tempdir_tests).

-include_lib("eunit/include/eunit.hrl").

tempdir_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_name())
    ,?_test(t_mktmp())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_name() ->
  DirPath  = tempdir:name([{prefix, "toto"}, {path, "tata"}]),
  ?assertMatch("tata", filename:dirname(DirPath)),
  ?assertMatch({match, _}, re:run(filename:basename(DirPath), "toto.{20}$")).

t_mktmp() ->
  TmpDir1 = tempdir:mktmp(fun(TmpDir) ->
    ?assert(filelib:is_dir(TmpDir)),
    TmpDir
  end),
  ?assertNot(filelib:is_dir(TmpDir1)),

  TmpDir2 = tempdir:mktmp([{remove, false}], fun(TmpDir) ->
    ?assert(filelib:is_dir(TmpDir)),
    TmpDir
  end),
  ?assert(filelib:is_dir(TmpDir2)).

