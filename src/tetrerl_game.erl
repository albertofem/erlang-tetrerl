-module(tetrerl_game).
-author("albertofem").

-behaviour(gen_server).

-type game_type() :: single | multi.
-type game_state() :: term().

-export_type([
  game_type/0,
  game_state/0
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-callback init_state() -> game_state().
-callback get_state() -> game_state().

init(_) ->
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
