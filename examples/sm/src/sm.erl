-module(sm).
-export([start/1, stop/0]).

% Public API

-ifdef(simulation).
start(Param) ->
    application:set_env(eliot, simulation, true),
    application:start(eliot),
    eliot_simulator:start(sm_task, Param).
-else.
start(Param) ->
    eliot_api:set_node_name(Param),
    application:start(eliot),
    application:start(sm).
-endif.

stop() ->
    Res = application:stop(sm),
    application:stop(eliot),
    Res.

% Private API
