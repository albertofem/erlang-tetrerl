-module(tetrerl_game).
-author("albertofem").

-include("include/tetrerl.hrl").
-include("include/tetrerl_game.hrl").

-behaviour(gen_server).

-define(GAME_SERVER, ?MODULE).

-type game_type() :: single | multi.
-type game_state() :: playing | paused | finished.

-export_type([
  game_type/0,
  game_state/0
]).

% Public API
-export([
  start_game/1,
  process_packet/1
]).

% Gen server
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

% Behaviour callbacks
-callback handle_command(Name :: string(), GameState :: #game_state{}, Args :: list()) -> game_state().
-callback handle_command(Name :: string(), GameState :: #game_state{}) -> game_state().

start_game(DelegateServer) ->
  gen_server:start_link({local, ?GAME_SERVER}, ?GAME_SERVER, DelegateServer, []).

init(DelegateServer) ->
  GameState = #game_state{
    game = DelegateServer,
    board = [],
    points = 0,
    lines = 0,
    elapsed = 0
  },
  ?LOG_INFO("Starting game with state: ~tp", [GameState]),
  erlang:start_timer(10000, self(), fetch_state),
  {ok, GameState}.

%% Command server handling

handle_call({command, [Name, Args]}, _From, GameState) ->
  GameServer = GameState#game_state.game,
  NewGameState = GameServer:handle_command(Name, GameState, Args),
  {reply, ok, NewGameState};

handle_call({command, Name}, _From, GameState) ->
  GameServer = GameState#game_state.game,
  NewGameState = GameServer:handle_command(Name, GameState),
  {reply, ok, NewGameState};

handle_call(fetch_state, _From, GameState) ->
  ?LOG_INFO("Fetching game state..", []),
  {reply, ok, GameState}.

%% Packet processing

process_packet([[{<<"cmd">>, Name}, {<<"args">>, Args}]|Tail]) ->
  gen_server:call(?GAME_SERVER, {command, [Name, Args]}),
  process_packet(Tail);

process_packet([[{<<"cmd">>,Name}]]) ->
  gen_server:call(?GAME_SERVER, {command, Name});

process_packet([]) ->
  ?LOG_INFO("Processed packet", []),
  {reply, packet_finished};

process_packet(Packet) ->
  ?LOG_WARN("Empty or invalid packet: ~tp", [Packet]),
  {reply, invalid_packet}.

handle_info(fetch_state, GameState) ->
  tetrerl_controller:websocket_init([], GameState, []).

terminate(_, _) ->
  erlang:error(not_implemented).

code_change(_, _, _) ->
  erlang:error(not_implemented).

handle_cast(_, __) ->
  error(not_implemented).