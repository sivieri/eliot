-module({{appid}}).
-export([start/1, stop/0]).

% Public API

-ifdef(simulation).
start(Param) ->
    application:set_env(wsn, simulation, true),
    application:start(wsn),
    wsn_simulator:start({{appid}}_task, Param).
-else.
start(Param) ->
    wsn_api:set_node_name(Param),
    application:start(wsn),
    application:start({{appid}}).
-endif.

stop() ->
    Res = application:stop({{appid}}),
    application:stop(wsn),
    Res.

% Private API
