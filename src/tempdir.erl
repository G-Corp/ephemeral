%% @doc
%% @author Gregoire Lejeune <gl@finexkap.com>
%% @copyright 2014 Finexkap
%%
%% Erlang module for managing temporary files
%% @end
-module(tempdir).

-export([
         name/0,
         name/1,
         mktmp/1,
         mktmp/2
        ]).

-type tmpname_options() :: [tmpname_option()].
-type tmpname_option() :: {prefix, string()} | {path, string()}.

% @equiv name([])
name() ->
  name([]).
% @doc
% Get a temporary dir name
%
% Options:
%
% * <code>prefix</code> : temporary directory prefix (default: <code>tmp_</code>
% * <code>path</code> : temp file path (default: <code>ostemp:dir()</code>)
% @end
-spec name(tmpname_options()) -> string().
name(Options) ->
  Options1 = maps:from_list(Options),
  Prefix = maps:get(prefix, Options1, "tmp_"),
  Path = maps:get(path, Options1, ostemp:dir()),
  filename:join([Path, Prefix ++ bucrandom:randstr(20)]).

% @equiv mktmp([], Fun)
mktmp(Fun) ->
  mktmp([], Fun).
% @doc
% Create a temporary directory
%
% Options:
%
% * <code>prefix</code> : temporary directory prefix (default: <code>tmp_</code>
% * <code>path</code> : temp file path (default: <code>ostemp:dir()</code>)
% * <code>remove</code> : remove the temp dir (default: <code>true</code>)
% @end
mktmp(Options, Fun) when is_list(Options), is_function(Fun, 1) ->
  Dir = name(Options),
  case bucfile:make_dir(Dir) of
    ok ->
      Result = Fun(Dir),
      _ = case buclists:keyfind(remove, 1, Options, true) of
            true -> bucfile:remove_recursive(Dir);
            _ -> ok
          end,
      Result;
    E -> E
  end.

