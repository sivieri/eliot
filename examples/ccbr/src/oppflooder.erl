-module(oppflooder).
-export([start/1, stop/0]).

% Public API

-ifdef(simulation).
start(Param) ->
    application:set_env(eliot, simulation, true),
    application:start(eliot),
    wsn_simulator:start(oppflooder_task, Param).
-else.
start(Param) ->
    eliot_api:set_node_name(Param),
    application:start(eliot),
    application:start(oppflooder).
-endif.

stop() ->
    Res = application:stop(oppflooder),
    application:stop(eliot),
    Res.

% Private API
