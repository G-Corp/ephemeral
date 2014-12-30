%% @doc
%% @author Gregoire Lejeune <gl@finexkap.com>
%% @copyright 2014 Finexkap
%%
%% Erlang module for managing temporary files
%% @end
-module(ostemp).
-include_lib("kernel/include/file.hrl").

-export([
         dir/0
        ]).

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
-spec dir() -> string() | false.
dir() ->
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

write_tmp_dir(Path) ->
  case file:read_file_info(Path) of
    {ok, #file_info{type = directory, access = Access}} when Access =:= read_write; Access =:= write ->
      Path;
    _ -> false
  end.
