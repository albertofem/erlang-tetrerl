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
  {ok, GameState}.

%% Command server handling

handle_call({command, [Name, Args]}, _From, GameState) ->
  GameServer = GameState#game_state.game,
  ?LOG_INFO("Calling module ~tp command ~tp with args ~tp", [GameServer, Name, Args]),
  NewGameState = GameServer:handle_command(Name, GameState, Args),
  ?LOG_INFO("Processed command. New state: ~tp", [NewGameState]),
  {reply, ok, NewGameState};

handle_call({command, Name}, _From, GameState) ->
  GameServer = GameState#game_state.game,
  ?LOG_INFO("Calling module ~tp command ~tp with no args", [GameServer, Name]),
  NewGameState = GameServer:handle_command(Name, GameState),
  ?LOG_INFO("Processed command. New state: ~tp", [NewGameState]),
  {reply, ok, NewGameState}.

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

handle_info(_, _) ->
  erlang:error(not_implemented).

terminate(_, _) ->
  erlang:error(not_implemented).

code_change(_, _, _) ->
  erlang:error(not_implemented).

handle_cast(_, __) ->
  error(not_implemented).