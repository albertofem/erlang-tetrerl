-module(tetrerl_multi).
-author("albertofem").

-behaviour(tetrerl_game).
-behaviour(gen_event).

-record(enemy_state, {
  board :: term()
}).

-export([
  init_state/0,
  get_state/0,
  get_enemy_state/0,
  link_enemy_server/1,
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

init_state() ->
  erlang:error(not_implemented).

get_state() ->
  erlang:error(not_implemented).

get_enemy_state() ->
  erlang:error(not_implemented).

link_enemy_server(_Pid) ->
  erlang:error(not_implemented).

init(_InitArgs) ->
  erlang:error(not_implemented).

handle_event(_Event, _State) ->
  erlang:error(not_implemented).

handle_call(_Request, _State) ->
  erlang:error(not_implemented).

handle_info(_Info, _State) ->
  erlang:error(not_implemented).

terminate(_Args, _State) ->
  erlang:error(not_implemented).

code_change(_OldVsn, _State, _Extra) ->
  erlang:error(not_implemented).
