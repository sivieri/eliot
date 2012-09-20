-module(miniapp_app).
-behaviour(application).
-export([start/2, stop/1]).

% Public API

start(_StartType, _StartArgs) ->
    miniapp_sup:start_link().

stop(_State) ->
    ok.

% Private API
