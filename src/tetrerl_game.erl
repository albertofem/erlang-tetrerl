-module(tetrerl_game).
-author("albertofem").

-behaviour(gen_server).

-define(GAME_SERVER, ?MODULE).

-record(command, {
  name :: term(),
  args :: list()
}).

-record(state, {
  board :: board(),
  points :: number(),
  lines :: number(),
  elapses :: number()
}).

-type board() :: [term()].
-type game_type() :: single | multi.
-type game_state() :: playing | paused | finished.

-export_type([
  game_type/0,
  game_state/0
]).

% Public API
-export([
  start_game/1,
  handle_command/0
]).

% Gen server
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-callback init_state(#state{}) -> game_state().
-callback process_command(#command{}) -> game_state().

start_game(Module) ->
  gen_server:start_link({local, ?GAME_SERVER}, Module, [], []).

init(_Args) ->
  erlang:error(not_implemented).

handle_call(_, _, _) ->
  erlang:error(not_implemented).

handle_cast(_, _) ->
  erlang:error(not_implemented).

handle_info(_, _) ->
  erlang:error(not_implemented).

terminate(_, _) ->
  erlang:error(not_implemented).

code_change(_, _, _) ->
  erlang:error(not_implemented).


handle_command() ->
  error(not_implemented).