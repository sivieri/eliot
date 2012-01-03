%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @reference <a href="http://portal.acm.org/citation.cfm?id=1251177">Trickle</a>
-module(trickle).
-export([start/1, stop/0]).

% Public API

start(Node) ->
    application:start(wsn),
    application:start(trickle),
    wsn_api:set_node_name(Node).

stop() ->
    Res = application:stop(trickle),
    application:stop(wsn),
    Res.

% Private API
