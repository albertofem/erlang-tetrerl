-module(tetrerl).
-author("albertofem").

-export([start/0]).

-spec(start() -> ok | {error, Reason :: term()}).
start() ->
  application:ensure_all_started(tetrerl, permanent).