-module(tetrerl_player).
-author("albertofem").

-include("include/tetrerl.hrl").

-behaviour(supervisor).

-define(PLAYER_SERVER, ?MODULE).

-export([
  process_message/1,
  start_link/0,
  init/1,
  code_change/3,
  handle_call/3,
  handle_info/2,
  terminate/2
]).

init(_) ->
  Procs = [
    {session, {tetrerl_session, start_link, []}, transient, brutal_kill, worker, [tetrerl_session]},
    {lobby, {tetrerl_lobby, start_link, []}, transient, brutal_kill, worker, [tetrerl_lobby]}
  ],
  {ok, {{one_for_one, 5, 10}, Procs}}.

process_message([{<<"msg">>, <<"start_single_game">>}]) ->
  supervisor:start_child(?PLAYER_SERVER, {single,
    {tetrerl_single, start_game, []}, transient, brutal_kill, worker, [tetrerl_single]}
  ),
  tetrerl_session:init_game_session(single);

process_message([{<<"msg">>, <<"packet">>}, {<<"args">>, Commands}]) ->
  tetrerl_session:process_packet(Commands),
  {true, []};

process_message(_) ->
  ?LOG_INFO("Invalid message", []),
  {false, <<"Invalid message">>}.

start_link() ->
  ?LOG_INFO("Starting player server...", []),
  supervisor:start_link({local, ?PLAYER_SERVER}, ?MODULE, []).

handle_call(_, _, _) ->
  erlang:error(not_implemented).

handle_info(_, _) ->
  erlang:error(not_implemented).

terminate(_, _) ->
  erlang:error(not_implemented).

code_change(_, _, _) ->
  erlang:error(not_implemented).

