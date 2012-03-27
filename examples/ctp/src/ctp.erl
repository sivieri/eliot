-module(ctp).
-export([start/1, stop/0]).

% Public API

-ifdef(simulation).
start(Param) ->
    application:set_env(eliot, simulation, true),
    application:start(eliot),
    wsn_simulator:start(ctp_task, Param).
-else.
start(Param) ->
    eliot_api:set_node_name(Param),
    application:start(eliot),
    application:start(ctp).
-endif.

stop() ->
    Res = application:stop(ctp),
    application:stop(eliot),
    Res.

% Private API
