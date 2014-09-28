-module(tetrerl_single).
-author("albertofem").

-include("include/tetrerl.hrl").

-define(SINGLE_GAME, ?MODULE).

-behaviour(tetrerl_game).

-export([
  process_packet/1
]).

-export([
  start_game/0,
  get_state/0,
  handle_command/3,
  handle_command/2
]).


start_game() ->
  tetrerl_game:start_game(?SINGLE_GAME).

process_packet(Commands) ->
  tetrerl_game:process_packet(Commands).

get_state() ->
  erlang:error(not_implemented).

handle_command(Name, _GameState, _Args) ->
  ?LOG_INFO("Processing message in single player: ~tp", [Name]).

handle_command(Name, _GameState) ->
  ?LOG_INFO("Processing message in single player: ~tp", [Name]).