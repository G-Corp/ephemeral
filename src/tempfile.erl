%% @doc
%% @author Gregoire Lejeune <gl@finexkap.com>
%% @author Grégoire Lejeune <gregoire.lejeune@botsunit.com>
%% @copyright 2014 Finexkap, 2016 BotsUnit
%%
%% Erlang module for managing temporary files
%% @end
-module(tempfile).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([name/1, name/2]).

-type tmpname_options() :: [tmpname_option()].
-type tmpname_option() :: {ext, string()} | {path, string()}.

% @hidden
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
  _ = temp_utils:ensure_started(),
  gen_server:call(?SERVER, {name, Prefix, Options}).

% @hidden
init(Args) ->
  _ = random:seed(erlang:system_time(micro_seconds)),
  {ok, Args}.

% @hidden
handle_call({name, Prefix, Options}, _From, State) ->
  Options1 = maps:from_list(Options),
  Ext = case maps:get(ext, Options1, ".tmp") of
          [$.|_] = Ext1 -> Ext1;
          Ext2 -> [$.|Ext2]
        end,
  Path = maps:get(path, Options1, ostemp:dir()),
  {reply, filename:join([Path, Prefix ++ temp_utils:randstr(20) ++ Ext]), State};
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

