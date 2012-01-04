%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @reference <a href="http://portal.acm.org/citation.cfm?id=1251177">Trickle</a>
-module(trickle).
-export([start/1, stop/0]).

% Public API
-ifdef(simulation).
start(Param) ->
    application:set_env(wsn, simulation, true),
    application:start(wsn),
    wsn_simulator:start(trickle_task, Param).
-else.
start(Param) ->
    wsn_api:set_node_name(Param),
    application:start(wsn),
    application:start(trickle).
-endif.

stop() ->
    Res = application:stop(trickle),
    application:stop(wsn),
    Res.

% Private API
