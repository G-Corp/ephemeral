%% @doc
%% @author Gregoire Lejeune <gl@finexkap.com>
%% @copyright 2014 Finexkap
%%
%% Erlang module for managing temporary files
%% @end
-module(tempfile).

-export([
         name/1,
         name/2
        ]).

-type tmpname_options() :: [tmpname_option()].
-type tmpname_option() :: {ext, string()} | {path, string()}.

% @equiv name(Prefix, [])
name(Prefix) ->
  name(Prefix, []).
% @doc
% Get a temporary file name
%
% Options:
%
% * <code>ext</code> : temp file extension (default: <code>.tmp</code>)
% * <code>path</code> : temp file path (default: <code>ostemp:dir()</code>)
%
% Examples:
%
% <pre lang="erlang">
% 1> tempfile:name("prefix_").
% "/tmp/prefix_ZL7YmS5HRQodKpOfEAaO.tmp"
% 2> tempfile:name("prefix_", [{ext, "toto"}]).
% "/tmp/prefix_RbmrP0wsde4NNwhBzzer.toto"
% 3> tempfile:name("prefix_", [{ext, ".toto"}]).
% "/tmp/prefix_0OSbH34VQlLbVtbSHFtj.toto"
% 4> tempfile:name("prefix_", [{ext, ".toto"}, {path, "."}]).
% "./prefix_Sa7BFnEzS6h862jmXQdy.toto"
% </pre>
% @end
-spec name(string(), tmpname_options()) -> string().
name(Prefix, Options) ->
  Options1 = maps:from_list(Options),
  Ext = case maps:get(ext, Options1, ".tmp") of
          [$.|_] = Ext1 -> Ext1;
          Ext2 -> [$.|Ext2]
        end,
  Path = maps:get(path, Options1, ostemp:dir()),
  filename:join([Path, Prefix ++ temp_utils:randstr(20) ++ Ext]).

