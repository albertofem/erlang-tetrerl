-module(tetrerl_ping).
-author("albertofem").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([
  ping/0,
  start_link/0,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]
).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

ping() ->
  gen_server:call(?SERVER, ping).

handle_call(ping, _From, State) ->
  {reply, pong, State}.

init(_) ->
  {ok, []}.

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

handle_info(ping, _State) ->
  erlang:error(not_implemented).

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, _State, _Extra) ->
  erlang:error(not_implemented).
