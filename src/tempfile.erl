%% @doc
%% @author Gregoire Lejeune <gl@finexkap.com>
%% @copyright 2014 Finexkap
%%
%% Erlang module for managing temporary files
%% @end
-module(tempfile).
-include_lib("kernel/include/file.hrl").
-define(CHARS, "azertyuiopqsdfghjklmwxcvbnAZERTYUIOPQSDFGHJKLMWXCVBN1234567890").

-export([
         os_tmp_dir/0,
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
% * <code>path</code> : temp file path (default: <code>os_tmp_dir()</code>)
%
% Examples:
%
%     > tempfile:name("prefix_").
%     "/tmp/prefix_ZL7YmS5HRQodKpOfEAaO.tmp"
%     > tempfile:name("prefix_", [{ext, "toto"}]).
%     "/tmp/prefix_RbmrP0wsde4NNwhBzzer.toto"
%     > tempfile:name("prefix_", [{ext, ".toto"}]).
%     "/tmp/prefix_0OSbH34VQlLbVtbSHFtj.toto"
%     > tempfile:name("prefix_", [{ext, ".toto"}, {path, "."}]).
%     "./prefix_Sa7BFnEzS6h862jmXQdy.toto"
% @end
-spec name(string(), tmpname_options()) -> string().
name(Prefix, Options) ->
  Options1 = maps:from_list(Options),
  Ext = case maps:get(ext, Options1, ".tmp") of
          [$.|_] = Ext1 -> Ext1;
          Ext2 -> [$.|Ext2]
        end,
  Path = maps:get(path, Options1, os_tmp_dir()),
  filename:join([Path, Prefix ++ randstr(20) ++ Ext]).

% @doc
% Returns a writable temporary directory. 
%
% Searches for directories in the following order:
%
% 1. the directory named by the <code>TMPDIR</code> environment variable 
% 2. the directory named by the <code>TEMP</code> environment variable 
% 3. the directory named by the <code>TMP</code> environment variable 
% 4. <code>C:\TMP</code> on Windows or <code>/tmp</code> on Unix 
% 5. as a last resort, the current working directory
%
% Returns <code>false</code> if none of the above are writable
% @end
os_tmp_dir() ->
  case os:getenv("TMPDIR") of
    false -> 
      case os:getenv("TEMP") of
        false -> 
          case os:getenv("TMP") of
            false -> 
              case write_tmp_dir("/tmp") of
                false -> 
                  Cwd = case file:get_cwd() of
                          {ok, Dir} -> Dir;
                          _ -> "."
                        end,
                  case write_tmp_dir(Cwd) of
                    false -> false;
                    LTmp -> LTmp
                  end;
                STmp -> STmp
              end;
            Tmp -> Tmp
          end;
        Temp -> Temp
      end;
    Tmpdir -> Tmpdir
  end.

% Private

randstr(Size) ->
  [lists:sublist(?CHARS, random:uniform(length(?CHARS)), 1) || _ <- lists:seq(1, Size)].

write_tmp_dir(Path) ->
  case file:read_file_info(Path) of
    {ok, #file_info{type = directory, access = Access}} when Access =:= read_write; Access =:= write ->
      Path;
    _ -> false
  end.
