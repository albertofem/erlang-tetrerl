-module(tetrerl_player).
-author("albertofem").

-include("include/tetrerl.hrl").

-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([
  start_link/0,
  init/1
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