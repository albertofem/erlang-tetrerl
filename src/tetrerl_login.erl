-module(tetrerl_login).
-author("albertofem").

-behaviour(gen_server).

%% Api
-export([
  login/1
]).

%% Gen server
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

login(_UserId) ->
  error(not_implemented).

init(_Args) ->
  erlang:error(not_implemented).

handle_call(_Request, _From, _State) ->
  erlang:error(not_implemented).

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).

handle_info(_Info, _State) ->
  erlang:error(not_implemented).

terminate(_Reason, _State) ->
  erlang:error(not_implemented).

code_change(_OldVsn, _State, _Extra) ->
  erlang:error(not_implemented).