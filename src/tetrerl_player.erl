-module(tetrerl_player).
-author("albertofem").

-include("include/tetrerl.hrl").

-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([
  start_link/0,
  init/1,
  code_change/3,
  process/2,
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

start_link() ->
  ?LOG_INFO("Starting player server...", []),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

process(<<"login">>, _Args) ->
  ?LOG_INFO("Logging in user", []),
  tetrerl_session:to_state(idle).

handle_call(_, _, _) ->
  erlang:error(not_implemented).

handle_info(_, _) ->
  erlang:error(not_implemented).

terminate(_, _) ->
  erlang:error(not_implemented).

code_change(_, _, _) ->
  erlang:error(not_implemented).