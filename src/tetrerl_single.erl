-module(tetrerl_single).
-author("albertofem").

-behaviour(tetrerl_game).

-export([
  start_game/0,
  get_state/0,
  init_state/1,
  process_commands/1
]).

start_game() ->
  tetrerl_game:start_game(?MODULE).

get_state() ->
  erlang:error(not_implemented).

init_state(_) ->
  erlang:error(not_implemented).

process_commands(_Commands) ->
  erlang:error(not_implemented).
