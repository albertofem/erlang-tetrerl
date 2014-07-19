-module(tetrerl_ping).
-author("albertofem").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([
  ping/0,
  start_link/0,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-spec(start_link() -> atom()).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(ping() -> {ok, pong}).

ping() ->
  gen_server:call(?SERVER, ping).

-spec(handle_call(term(), term(), State :: atom())
      -> {reply, pong, atom()}).

handle_call(ping, _From, State) ->
  {reply, pong, State}.

-spec(init(any()) -> {ok, alive}).

init(_) ->
  {ok, alive}.

-spec(handle_cast(term(), State :: atom())
      -> {noreply, atom()}).

handle_cast(_Request, State) ->
  {noreply, State}.

-spec(handle_info(atom(), State :: atom())
      -> {noreply, atom()}).

handle_info(ping, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
