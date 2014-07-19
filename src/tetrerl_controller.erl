-module(tetrerl_controller).
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
    {ping, {tetrerl_ping, start_link, []}, transient, brutal_kill, worker, [tetrerl_ping]},
    {message_handler, {}}
  ],
  {ok, {{one_for_one, 5, 10}, Procs}}.

websocket_init(_Transport, Req, _Opts) ->
  ?LOG_INFO("Initialized websocket session", []),
  {ok, Req, []}.

-spec(websocket_handle({text, <<>>}, term(), term())
      -> {reply, <<>>, term(), term()}).

-spec(websocket_handle({text, Message :: tetrerl_message:message()}, term(), term())
      -> {reply, <<>>, term(), term()}).

websocket_handle({text, <<"ping">>}, Req, _) ->
  PingServerResponse = tetrerl_ping:ping(),
  ?LOG_INFO("Ping server response: ~w", [PingServerResponse]),
  case PingServerResponse of
    pong -> {reply, {text, <<"pong">>}, Req, []};
    _ -> {reply, {text, <<"ERROR: no ping server active">>}, Req, []}
  end;

websocket_handle({text, <<"player">>}, Req, _) ->
  tetrert_player:message(Message),
  {reply, {text, <<"Ack.">>}, Req, []};

websocket_handle({text, _}, Req, _) ->
  {reply, {text, <<"...">>}, Req, []}.

websocket_info(_, Req, _) ->
  {reply, {text, <<"...">>}, Req, []}.

websocket_terminate(Reason, _, _) ->
  ?LOG_INFO("Terminated websocket session with reason: ~w", [Reason]),
  ok.