-module(tetrerl_handler).
-author("albertofem").

-include("include/tetrerl.hrl").

-behaviour(cowboy_websocket_handler).
-behaviour(supervisor).

-export([
  init/3,
  init/1,
  start_link/0,
  websocket_init/3,
  websocket_handle/3,
  websocket_info/3,
  websocket_terminate/3
]).

init({tcp, http}, _Req, _Opts) ->
  start_link(),
  {upgrade, protocol, cowboy_websocket}.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [
    {ping, {tetrerl_ping, start_link, []}, transient, brutal_kill, worker, [tetrerl_ping]}
  ],
  {ok, {{one_for_one, 5, 10}, Procs}}.

websocket_init(_Transport, Req, _Opts) ->
  ?LOG_INFO("Initialized websocket session", []),
  {ok, Req, []}.

websocket_handle({ping, _}, _, _) ->
  tetrerl_ping:ping();
websocket_handle({text, <<"ping">>}, Req, _) ->
  PingServerResponse = tetrerl_ping:ping(),
  ?LOG_INFO("Ping server response: ~w", [PingServerResponse]),
  case PingServerResponse of
    pong -> {reply, {text, <<"pong">>}, Req, []};
    _ -> {reply, {text, <<"ERROR: no ping server active">>}, Req, []}
  end;
websocket_handle({text, _}, Req, _) ->
  {reply, {text, <<"Not implemented">>}, Req, []}.

websocket_info(_, _, _) ->
  erlang:error(not_implemented).

websocket_terminate(Reason, _, _) ->
  ?LOG_INFO("Terminated websocket session with reason: ~w", [Reason]),
  ok.