-module(tetrerl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    io:write("hello there~n"),
    tetrerl_sup:start_link().

stop(_State) ->
    ok.
