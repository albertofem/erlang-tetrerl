-module(tetrerl_player).
-author("albertofem").

-include("include/tetrerl.hrl").

-behaviour(supervisor).

-type login_state() :: anonymous | logged.
-type game_view() :: lobby | idle.
-type game_mode() :: single | multi.

-define(SERVER, ?MODULE).

-record(player, {
  state :: login_state(),
  view :: game_view(),
  mode :: game_mode()
}).

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
    {login, {tetrerl_login, start_link, []}, transient, brutal_kill, worker, [tetrerl_session]},
    {session, {tetrerl_session, start_link, []}, transient, brutal_kill, worker, [tetrerl_session]},
    {lobby, {tetrerl_lobby, start_link, []}, transient, brutal_kill, worker, [tetrerl_lobby]}
  ],
  {ok, {{one_for_one, 5, 10}, Procs}}.

process_message([{<<"command">>, <<"login">>}, {<<"args">>, [UserID]}]) ->
  ?LOG_INFO("Logging in user", []),
  tetrerl_login:login(UserID);

process_message([{<<"command">>, <<"start_single_game">>}, {<<"args">>, _Args}]) ->
  ?LOG_INFO("Starting single player game", []),
  tetrerl_single:start_game();

process_message([{<<"command">>, <<"packet">>}, {<<"args">>, Commands}]) ->
  ?LOG_INFO("Executing packet", []),
  tetrerl_single:process_commands(Commands).

start_link() ->
  ?LOG_INFO("Starting player server...", []),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

handle_call(_, _, _) ->
  erlang:error(not_implemented).

handle_info(_, _) ->
  erlang:error(not_implemented).

terminate(_, _) ->
  erlang:error(not_implemented).

code_change(_, _, _) ->
  erlang:error(not_implemented).

