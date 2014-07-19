-module(tetrerl_message).
-author("albertofem").

-behaviour(gen_server).

-type message_raw() :: {Message :: message, RawContent :: <<>>}.
-type message() :: {Message :: message, Content :: jsx:json_term()}.

-type response() :: {Result :: success} | {Reason :: error, Reason :: <<>> | atom()}.

-export_type([message/0]).

-record(message_session, {
  current :: number(),
  responses :: [response()],
  last_response :: response()
}).

-define(SERVER, ?MODULE).

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
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(process(message_raw()) -> response()).

process(MessageRaw) ->
  gen_server:call(?SERVER, {process, MessageRaw}).

handle_call({process, MessageRaw}, {Pid, _}, MessageSession) ->
  {Response, NewMessageSession} = do_process_message(MessageRaw, MessageSession),
  {reply, Response, NewMessageSession}.

do_process_message(MessageRaw, MessageSession) ->
  case jsx:is_json(MessageRaw) of
    false -> throw(invalid_message)
  end,
  Message = jsx:decode(MessageRaw),
  Response = execute_message(Message),
  NewMessageSession = #message_session{
    current = MessageSession#message_session.current + 1,
    responses = MessageSession#message_session.responses ++ [Response],
    last_response = Response
  },
  {Response, NewMessageSession}.

init(Args) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).

handle_info(Info, State) ->
  erlang:error(not_implemented).

terminate(Reason, State) ->
  erlang:error(not_implemented).

code_change(OldVsn, State, Extra) ->
  erlang:error(not_implemented).
