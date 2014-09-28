-module(tetrerl_session).
-author("albertofem").

-include("include/tetrerl.hrl").

-behaviour(gen_server).

-define(SESSION_SERVER, ?MODULE).

-type game_view() :: lobby | idle.
-type game_mode() :: none | single | multi.

-record(player, {
  id :: number(),
  pid :: pid(),
  view :: game_view(),
  mode :: game_mode()
}).

-record(session, {
  id :: number(),
  player :: #player{id :: number(), pid :: pid(), view :: game_view(), mode :: game_mode()}
}).

% API
-export([
  process_packet/1,
  init_game_session/1
]).

%% Gen server
-export([
  init/1,
  start_link/0,
  handle_info/2,
  handle_cast/2,
  handle_call/3,
  terminate/2,
  code_change/3
]).

start_link() ->
  ?LOG_INFO("Starting session server...", []),
  gen_server:start_link({local, ?SESSION_SERVER}, ?MODULE, [], []).

init(_) ->
  Player = #player{
    id = erlang:phash2({node(), now()}),
    view = lobby,
    mode = none
  },
  Session = #session{
    id = erlang:phash2({node(), now()}),
    player = Player
  },
  {ok, Session}.

init_game_session(Mode) ->
  gen_server:call(?SESSION_SERVER, {init_game_session, Mode}, 5).

process_packet(Commands) ->
  gen_server:cast(?SESSION_SERVER, {packet, Commands}).

handle_call({init_game_session, Mode}, From, Session) ->
  Player = Session#session.player#player{
    id = From,
    pid = From,
    mode = Mode
  },
  NewSession = Session#session{
    player = Player
  },
  ?LOG_INFO("Initialized game session: ~tp", [NewSession]),
  {reply, {true, [{<<"session_id">>, Session#session.id}]}, NewSession}.

handle_cast({packet, Commands}, Session) ->
  case Session#session.player#player.mode of
    single ->
      tetrerl_single:process_packet(Commands),
      {noreply, Session};
    _ ->
      ?LOG_WARN("No game mode selected, ignoring packet", []),
      {noreply, Session}
  end.

handle_info(_, _) ->
  erlang:error(not_implemented).

terminate(Reason, _State) ->
  ?LOG_INFO("Terminating session: ~w", [Reason]),
  {ok}.

code_change(_, _, _) ->
  erlang:error(not_implemented).