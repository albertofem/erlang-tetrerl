-module(tetrerl_protocol).
-author("albertofem").

-include("include/tetrerl.hrl").

-export([
  parse/1
]).

parse(RawMessage) ->
  case jsx:is_json(RawMessage) of
    true -> {success, do_parse_json(RawMessage)};
    false -> {error, invalid_json}
  end.

do_parse_json(JsonMessage) ->
  Message = jsx:decode(JsonMessage),
  ?LOG_INFO("Parsed json: ~tp", [Message]),
  Message.