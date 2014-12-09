-module(gcm_app).

-behaviour(application).

-export([start/0]).
-export([start/2, stop/1]).

start() ->
    application:start(inets),
    application:start(jsx),
    application:start(gcm).

start(_StartType, _StartArgs) ->
    gcm_sup:start_link().

stop(_State) ->
    ok.
