-module(tetrerl_single).
-author("albertofem").

-include("include/tetrerl.hrl").
-include("include/tetrerl_game.hrl").

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

handle_command(<<"update_board">>, GameState, Args) ->
  ?LOG_INFO("Processing update_board message in single player", []),
  NewGameState = GameState#game_state{
    board = Args
  },
  NewGameState;

handle_command(<<"move_y">>, GameState, _Args) ->
  ?LOG_INFO("Moving Y axis", []),
  GameState;

handle_command(<<"move_x">>, GameState, _Args) ->
  ?LOG_INFO("Moving X axis", []),
  GameState.

handle_command(_Name, _GameState) ->
  erlang:error(not_implemented).