%% @doc
%% @author Gregoire Lejeune <gl@finexkap.com>
%% @author Grégoire Lejeune <gregoire.lejeune@botsunit.com>
%% @copyright 2014 Finexkap, 2016 BotsUnit
%%
%% Erlang module for managing temporary files
%% @end
-module(tempdir).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([name/0, name/1, mktmp/1, mktmp/2]).

-type tmpname_options() :: [tmpname_option()].
-type tmpname_option() :: {prefix, string()} | {path, string()}.

% @hidden
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
  _ = temp_utils:ensure_started(),
  gen_server:call(?SERVER, {name, Options}).

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
  _ = temp_utils:ensure_started(),
  gen_server:call(?SERVER, {mktmp, Options, Fun}).

% @hidden
init(Args) ->
  _ = random:seed(erlang:system_time(micro_seconds)),
  {ok, Args}.

% @hidden
handle_call({name, Options}, _From, State) ->
  {reply, name1(Options), State};
handle_call({mktmp, Options, Fun}, _From, State) ->
  Dir = name1(Options),
  case bucfile:make_dir(Dir) of
    ok ->
      Result = Fun(Dir),
      _ = case buclists:keyfind(remove, 1, Options, true) of
            true ->
              bucfile:remove_recursive(Dir);
            _ ->
              ok
          end,
      {reply, Result, State};
    E -> {reply, E, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% @hidden
name1(Options) ->
  _ = temp_utils:ensure_started(),
  Options1 = maps:from_list(Options),
  Prefix = maps:get(prefix, Options1, "tmp_"),
  Path = maps:get(path, Options1, ostemp:dir()),
  filename:join([Path, Prefix ++ temp_utils:randstr(20)]).
