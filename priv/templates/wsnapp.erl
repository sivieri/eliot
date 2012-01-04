-module({{appid}}).
-export([start/1, stop/0]).

% Public API

start([Node, Config]) ->
    application:set_env(wsn, simulation, true),
    application:start(wsn),
    wsn_simulator:start(Node, Config);;
start(Node) ->
    wsn_api:set_node_name(Node),
    application:start(wsn),
    application:start({{appid}}).

stop() ->
    Res = application:stop({{appid}}),
    application:stop(wsn),
    Res.

% Private API
