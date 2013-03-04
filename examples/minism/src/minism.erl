-module(minism).
-export([start/1, stop/0]).

% Public API

-ifdef(simulation).
start(Param) ->
    application:set_env(eliot, simulation, true),
    application:start(eliot),
    eliot_simulator:start(minism_task, Param).
-else.
start(Param) ->
    eliot_api:set_node_name(Param),
    application:start(eliot),
    application:start(minism).
-endif.

stop() ->
    Res = application:stop(minism),
    application:stop(eliot),
    Res.

% Private API
