%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @reference <a href="http://portal.acm.org/citation.cfm?id=1251177">Trickle</a>
-module(trickle).
-export([start/1, stop/0]).

% Public API
-ifdef(simulation).
start(Param) ->
    application:set_env(eliot, simulation, true),
    application:start(eliot),
    eliot_simulator:start(trickle_task, Param).
-else.
start(Param) ->
    eliot_api:set_node_name(Param),
    application:start(eliot),
    application:start(trickle).
-endif.

stop() ->
    Res = application:stop(trickle),
    application:stop(eliot),
    Res.

% Private API
