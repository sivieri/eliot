%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Simulator.
-module(wsn_simulator).
-include("wsn.hrl").
-export([start/2, send/2, register/2]).

% Public API

start(Module, Config) ->
    Net = read_net(Config).

send(Dest, Msg) ->
    ok.

register(Name, Pid) ->
    ok.

% Private API
-spec(read_net(string()) -> net()).
read_net(Filename) ->
    {ok, Device} = file:open(Filename,[read]),
    read_net(Device, sets:new(), dict:new()).

-spec(read_net(pid(), set(), dict()) -> net()).
read_net(Device, Nodes, Gains) ->
    case io:get_line(Device, "") of
    "gain"++Rest ->
        [Node1, Node2, Gain] = string:tokens(Rest," \t"),
        {G,_}=string:to_float(Gain), % remove trailing CR and LF
        NewNodes = sets:add_element(wsn_api:nodeid(Node1), Nodes),
        NewGains = dict:store({wsn_api:nodeid(Node1), wsn_api:nodeid(Node2)}, G, Gains),
        read_net(Device, NewNodes, NewGains);
    _Else ->
        file:close(Device),
        SortedNodes = lists:sort(sets:to_list(Nodes)),
        {SortedNodes, Gains}
    end.
