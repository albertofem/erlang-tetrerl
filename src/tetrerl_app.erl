-module(tetrerl_app).
-author("albertofem").

-include("include/tetrerl.hrl").

-behaviour(application).

-import_type([
  start_type/0
]).

-export([
  start/2,
  stop/1,
  getenv/1
]).

start(_, _) ->
    ?LOG_INFO("Welcome to Tetrerl server backend", []),
    start_websocket(getenv(port)).

-spec(stop(State :: term()) -> ok).

stop(_) ->
    ok.

-spec(start_websocket(Port :: number()) -> {ok, term()}).

start_websocket(Port) ->
  Dispatch = cowboy_router:compile([
    {'_', [{"/", tetrerl_controller, []}]}
  ]),
  {Result, Message} = cowboy:start_http(http, 1000, [{port, Port}], [
    {env, [{dispatch, Dispatch}]}
  ]),
  ?LOG_INFO("Initialized websocket on port ~w", [Port]),
  {Result, Message}.

-spec(getenv(Key :: term()) -> term()).

getenv(Key) ->
  {ok, Value} = application:get_env(tetrerl, Key),
  Value.