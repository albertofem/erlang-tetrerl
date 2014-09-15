-module(tetrerl_session).
-author("albertofem").

-include("include/tetrerl.hrl").

-behaviour(gen_server).

-type state() :: lobby | idle | single | multi.

-define(SERVER, ?MODULE).

-record(session, {
  id :: number(),
  state :: state(),
  user_id :: number()
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
  process/1,
  to_state/1
]).

%% Internal API
-export([
  change_state/2
]).

start_link() ->
  ?LOG_INFO("Starting session server...", []),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
  Session = #session{
    id = erlang:phash2({node(), now()}),
    state = anonymous,
    user_id = null
  },
  {ok, Session}.

process(Message) ->
  ?LOG_INFO("Processing message: ~tp", [Message]),
  gen_server:cast(?MODULE, {message, Message}).

to_state(State) ->
  ?LOG_INFO("Processing session state change: ~w", [State]),
  gen_server:cast(?MODULE, {change_state, State}).

handle_cast({message, [{<<"msg">>, Message}, {<<"args">>, Args}]}, Session) ->
  ?LOG_INFO("Handling message, current session: ~w", [Session]),
  case Session#session.state of
    anonymous -> tetrerl_player:process(Message, Args);
    idle -> {noreply, Session};
    lobby -> {noreply, tetrerl_lobby:process(Message)};
    single -> {noreply, tetrerl_single:process(Message)};
    multi -> {noreply, tetrert_multi:process(Message)};
    _ -> {stop, invalid_state} %% do some kind of state recovery in terminate()
  end;
handle_cast({change_state, State}, Session) ->
  ?LOG_INFO("Changing user state from ~w to ~w", [Session#session.state, State]),
  case Session#session.state of
    State -> {noreply, Session};
    _ -> {noreply, change_state(State, Session)}
  end.

change_state(NextState, Session) ->
  NextSession = Session,
  NextSession = #session{
    id = Session#session.id,
    state = NextState,
    user_id = Session#session.user_id
  },
  NextSession.

handle_call(_, _, _) ->
  erlang:error(not_implemented).

handle_info(_, _) ->
  erlang:error(not_implemented).

terminate(Reason, _State) ->
  ?LOG_INFO("Terminating session: ~w", [Reason]),
  erlang:error(not_implemented).

code_change(_, _, _) ->
  erlang:error(not_implemented).
