-module(appliance_app).
-behaviour(application).
-export([start/2, stop/1]).

% Public API

start(_StartType, _StartArgs) ->
    appliance_sup:start_link().

stop(_State) ->
    ok.

% Private API