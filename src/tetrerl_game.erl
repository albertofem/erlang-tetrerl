-module(tetrerl_game).
-author("albertofem").

-include("include/tetrerl.hrl").

-behaviour(gen_server).

-define(GAME_SERVER, ?MODULE).

-record(state, {
  game :: module(),
  board :: board(),
  points :: number(),
  lines :: number(),
  elapsed :: number()
}).

-type board() :: [term()].
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

-callback handle_command(Name :: string(), GameState :: #state{}, Args :: list()) -> game_state().
-callback handle_command(Name :: string(), GameState :: #state{}) -> game_state().

start_game(DelegateServer) ->
  gen_server:start_link({local, ?GAME_SERVER}, ?GAME_SERVER, DelegateServer, []).

handle_call({command, [Name, Args]}, _From, GameState) ->
  GameServer = GameState#state.game,
  ?LOG_INFO("Calling module ~tp command ~tp with args ~tp", [GameServer, Name, Args]),
  GameServer:handle_command(Name, GameState, Args),
  {reply, ok, GameState};
handle_call({command, Name}, _From, GameState) ->
  GameServer = GameState#state.game,
  ?LOG_INFO("Calling module ~tp command ~tp with no args", [GameServer, Name]),
  GameServer:handle_command(Name, GameState),
  {reply, ok, GameState}.

process_packet([[{<<"cmd">>, Name}, {<<"args">>, Args}]|Tail]) ->
  gen_server:call(?GAME_SERVER, {command, [Name, Args]}),
  process_packet(Tail);
process_packet([[{<<"cmd">>,Name}]]) ->
  gen_server:call(?GAME_SERVER, {command, Name});
process_packet(Packet) ->
  ?LOG_WARN("Empty or invalid packet: ~tp", [Packet]),
  {reply, invalid_packet}.

init(DelegateServer) ->
  GameState = #state{
    game = DelegateServer,
    board = [],
    points = 0,
    lines = 0,
    elapsed = 0
  },
  ?LOG_INFO("Starting game with state: ~tp", [GameState]),
  {ok, GameState}.

handle_info(_, _) ->
  erlang:error(not_implemented).

terminate(_, _) ->
  erlang:error(not_implemented).

code_change(_, _, _) ->
  erlang:error(not_implemented).

handle_cast(_, __) ->
  error(not_implemented).