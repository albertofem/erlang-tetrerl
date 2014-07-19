-module(tetrerl_session).
-author("albertofem").

-include("include/tetrerl.hrl").

-behaviour(gen_server).

-type state() :: lobby | idle | single | multi.

-define(SERVER, ?MODULE).

-record(session, {
  id :: number(),
  state :: state()
}).

-export_type([state/0]).

%% API
-export([
  init/1,
  start_link/0,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3,
  process/1
]).

start_link() ->
  ?LOG_INFO("Starting session server...", []),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
  Session = #session{
    id = erlang:phash2({node(), now()}),
    state = idle
  },
  {ok, Session}.

process(Message) ->
  ?LOG_INFO("Processing message: ~tp", [Message]),
  gen_server:cast(?MODULE, {message, Message}).

handle_cast({message, Message}, Session) ->
  ?LOG_INFO("Handling message, current session: ~w", [Session]),
  case Session#session.state of
    idle -> {noreply, Session};
    lobby -> {noreply, tetrerl_lobby:process(Message)};
    single -> {noreply, tetrerl_single:process(Message)};
    multi -> {noreply, tetrert_multi:process(Message)};
    _ -> {stop, invalid_state} %% do some kind of state recovery in terminate()
  end.

handle_call(_, _, _) ->
  erlang:error(not_implemented).

handle_info(_, _) ->
  erlang:error(not_implemented).

terminate(_, _) ->
  erlang:error(not_implemented).

code_change(_, _, _) ->
  erlang:error(not_implemented).
