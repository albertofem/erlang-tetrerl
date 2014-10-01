-module(tetrerl_tetris_board).
-author("albertofem").

-include("include/tetrerl_game.hrl").
-include("include/tetrerl_tetris_board.hrl").

-define(BOARD_SERVER, ?MODULE).

-behaviour(gen_server).

%% External API
-export([
  move_y/1,
  move_x/2,
  start_board/0,
  get_next_piece/0
]).

%% Gen server API
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

start_board() ->
  gen_server:start_link({local, ?BOARD_SERVER}, ?BOARD_SERVER, [], []).

init(_) ->
  register_pieces(),
  BoardState = #board{},
  {ok, BoardState}.

register_pieces() ->
  LongPiece = #piece{
    probability = 1
  },
  Pieces = [LongPiece],
  ets:new(tetrerl_pieces, [named_table, protected, set, {keypos, 1}]),
  ets:insert(tetrerl_pieces, Pieces).

get_next_piece() ->
  gen_server:call(?BOARD_SERVER, get_next_piece).

move_x(Pos, Dir) ->
  gen_server:cast(?BOARD_SERVER, {move_x, {Pos, Dir}}).

move_y(Pos) ->
  gen_server:cast(?BOARD_SERVER, {move_y, {Pos}}).

handle_call(get_next_piece, _From, _BoardState) ->
  %% get tetrerl pieces from ets
  %% get a random number between 0 and lenght of pieces
  %% return piece id
  erlang:error(not_implemented).

handle_cast({move_x, {_Pos, _Dir}}, _BoardState) ->
  %% check we can move in the given direction
  %% check piece boundaries with board limits
  {ok};

handle_cast({move_y, {_Pos}}, _BoardState) ->
  %% check we can move down
  %% check piece boundaries with board limits
  %% check piece boundaries with already stacked pieces
  {ok}.

handle_info(_Info, _State) ->
  erlang:error(not_implemented).

terminate(_Reason, _State) ->
  erlang:error(not_implemented).

code_change(_OldVsn, _State, _Extra) ->
  erlang:error(not_implemented).
