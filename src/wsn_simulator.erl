%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Simulator.
-module(wsn_simulator).
-include("wsn.hrl").
-export([start/2, send/2, register/2, read_net/1, get_simname/1, get_simname/2, get_name/1]).

% Public API

start(Module, Config) ->
    {Nodes, Gains} = read_net(Config),
    wsn_sup:start_task(wsn_forwarder),
    wsn_forwarder:set_gains(Gains),
    lists:foreach(fun(NodeAddr) -> spawn(start_task(NodeAddr, Module, start_link, [])) end, Nodes).

start_task(NodeAddr, Module, Function, Args) ->
    wsn_api:set_node_name(nodeid(NodeAddr)),
    Module:Function(Args).

send(Dest, Msg) when is_atom(Dest) ->
    erlang:send(get_simname(Dest), Msg);
send(Dest, Msg) ->
    erlang:send(Dest, Msg).

register(Name, Pid) ->
    erlang:register(get_simname(Name), Pid).

-spec(read_net(string()) -> net()).
read_net(Filename) ->
    {ok, Device} = file:open(Filename, [read]),
    read_net(Device, sets:new(), dict:new()).

get_simname(Name) ->
    list_to_atom(atom_to_list(Name) ++ $_ ++ atom_to_list(wsn_api:get_node_name())).

get_simname(Name, NodeName) ->
    list_to_atom(atom_to_list(Name) ++ $_ ++ atom_to_list(NodeName)).

get_name(Name) ->
    NameString = atom_to_list(Name),
    PrevIdx = string:rchr(NameString, $_),
    SubStr = string:sub_string(NameString, 1, PrevIdx - 1),
    SecondIdx = string:rchr(SubStr, $_),
    FinalString = string:sub_string(NameString, SecondIdx + 1, string:len(NameString) - SecondIdx),
    list_to_atom(FinalString).

% Private API

nodeid(NodeAddr) ->
    list_to_atom(string:substr(atom_to_list(NodeAddr), 6)).

-spec(read_net(pid(), set(), dict()) -> net()).
read_net(Device, Nodes, Gains) ->
    case io:get_line(Device, "") of
        "gain" ++ Rest ->
            [Node1, Node2, Gain] = string:tokens(Rest, " \t"),
            {G, _} = string:to_float(Gain), % remove trailing CR and LF
            NewNodes = sets:add_element(wsn_api:nodeaddr(utils:to_int(Node1)), Nodes),
            NewGains = dict:store({wsn_api:nodeaddr(utils:to_int(Node1)), wsn_api:nodeaddr(utils:to_int(Node2))}, G, Gains),
            read_net(Device, NewNodes, NewGains);
        _Any ->
            file:close(Device),
            SortedNodes = lists:sort(sets:to_list(Nodes)),
            {SortedNodes, Gains}
    end.