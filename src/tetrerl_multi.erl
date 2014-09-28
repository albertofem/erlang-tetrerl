-module(tetrerl_multi).
-author("albertofem").

-behaviour(tetrerl_game).
-behaviour(gen_event).

%% -record(enemy_state, {
%%   board :: term()
%% }).

-export([
  handle_command/3,
  handle_command/2,
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

handle_command(_Name, _GameState, _Args) ->
  erlang:error(not_implemented).

handle_command(_Name, _GameState) ->
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
