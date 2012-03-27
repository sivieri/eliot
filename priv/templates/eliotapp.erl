-module({{appid}}).
-export([start/1, stop/0]).

% Public API

-ifdef(simulation).
start(Param) ->
    application:set_env(eliot, simulation, true),
    application:start(eliot),
    eliot_simulator:start({{appid}}_task, Param).
-else.
start(Param) ->
    eliot_api:set_node_name(Param),
    application:start(eliot),
    application:start({{appid}}).
-endif.

stop() ->
    Res = application:stop({{appid}}),
    application:stop(eliot),
    Res.

% Private API
