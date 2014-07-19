-module(tetrerl_player).
-author("albertofem").

-type player_state() :: {idle} | {playing, GameType :: tetrerl_game:game_type()}.

-behaviour(supervisor).
-behaviour(gen_event).

-export([
  handle_event/2,
  handle_call/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {
  state :: player_state()
}).

init(Args) ->
  erlang:error(not_implemented).

handle_event(Event, State) ->
  erlang:error(not_implemented).

handle_call(Request, State) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  erlang:error(not_implemented).

terminate(Args, State) ->
  erlang:error(not_implemented).

code_change(OldVsn, State, Extra) ->
  erlang:error(not_implemented).
