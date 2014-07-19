-module(tetrerl_single).
-author("albertofem").

-export([
  get_state/0,
  init_state/0
]).

-record(board, {
  piece_matrix :: [term()]
}).

-record(state, {
  board :: #board{},
  points :: number(),
  lines :: number(),
  elapses :: number()
}).

get_state() ->
  erlang:error(not_implemented).

init_state() ->
  NewState = #state{},
  NewState.
