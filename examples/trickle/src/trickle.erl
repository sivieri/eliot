%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @reference <a href="http://portal.acm.org/citation.cfm?id=1251177">Trickle</a>
-module(trickle).
-export([start/1, stop/0]).

% Public API

start([Node, Config]) ->
    application:set_env(wsn, simulation, true),
    application:start(wsn),
    wsn_simulator:start(Node, Config);
start(Node) ->
    wsn_api:set_node_name(Node),
    application:start(wsn),
    application:start(trickle).

stop() ->
    Res = application:stop(trickle),
    application:stop(wsn),
    Res.

% Private API
