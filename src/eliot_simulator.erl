%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Simulator.
-module(eliot_simulator).
-include("eliot.hrl").
-export([start/2, send/2, send_after/3, register/2, spawn/1, spawn/3, spawn_link/1, spawn_link/3, read_net/1, get_simname/1, get_simname/2, get_name/1]).

% Public API

start(Module, Config) ->
    {Nodes, Gains} = read_net(Config),
    eliot_sup:start_task(eliot_forwarder),
    eliot_forwarder:set_gains(Gains),
    lists:foreach(fun(NodeAddr) -> erlang:spawn(fun() -> start_task(NodeAddr, Module, start_link) end) end, Nodes).

start_task(NodeAddr, Module, Function) ->
    eliot_api:set_node_name(nodeid(NodeAddr)),
    Module:Function().

send(Dest, Msg) when is_atom(Dest) ->
    erlang:send(get_simname(Dest), Msg);
send(Dest, Msg) ->
    erlang:send(Dest, Msg).

send_after(Time, Dest, Msg) when is_atom(Dest) ->
    erlang:send_after(Time, get_simname(Dest), Msg);
send_after(Time, Dest, Msg) ->
    erlang:send_after(Time, Dest, Msg).

register(Name, Pid) ->
    erlang:register(get_simname(Name), Pid).

spawn(Fun) ->
    Name = eliot_api:get_node_name(),
    erlang:spawn(fun() -> spawn_helper(Name, Fun) end).

spawn(M, F, A) ->
    Name = eliot_api:get_node_name(),
    erlang:spawn(fun() -> spawn_helper(Name, M, F, A) end).

spawn_link(Fun) ->
    Name = eliot_api:get_node_name(),
    erlang:spawn_link(fun() -> spawn_helper(Name, Fun) end).

spawn_link(M, F, A) ->
    Name = eliot_api:get_node_name(),
    erlang:spawn_link(fun() -> spawn_helper(Name, M, F, A) end).

-spec(read_net(string()) -> net()).
read_net(Filename) ->
    {ok, Device} = file:open(Filename, [read]),
    read_net(Device, sets:new(), dict:new()).

get_simname(Name) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(eliot_api:get_node_name())).

get_simname(Name, NodeName) ->
    list_to_atom(atom_to_list(Name) ++ "_" ++ atom_to_list(NodeName)).

get_name(Name) ->
    NameString = atom_to_list(Name),
    PrevIdx = string:rchr(NameString, $_),
    SubStr = string:sub_string(NameString, 1, PrevIdx - 1),
    SecondIdx = string:rchr(SubStr, $_),
    FinalString = string:sub_string(NameString, SecondIdx + 1, string:len(NameString) - SecondIdx),
    list_to_atom(FinalString).

% Private API

spawn_helper(Name, Fun) ->
    eliot_api:set_node_name(Name),
    Fun().

spawn_helper(Name, Module, Function, Args) ->
    eliot_api:set_node_name(Name),
    erlang:apply(Module, Function, Args).

nodeid(NodeAddr) ->
    list_to_atom(string:substr(atom_to_list(NodeAddr), 6)).

-spec(read_net(pid(), set(), dict()) -> net()).
read_net(Device, Nodes, Gains) ->
    case io:get_line(Device, "") of
        "gain" ++ Rest ->
            [Node1, Node2, Gain] = string:tokens(Rest, " \t"),
            {G, _} = string:to_float(Gain), % remove trailing CR and LF
            NewNodes = sets:add_element(eliot_api:nodeaddr(utils:to_int(Node1)), Nodes),
            NewGains = dict:store({eliot_api:nodeaddr(utils:to_int(Node1)), eliot_api:nodeaddr(utils:to_int(Node2))}, G, Gains),
            read_net(Device, NewNodes, NewGains);
        _Any ->
            file:close(Device),
            SortedNodes = lists:sort(sets:to_list(Nodes)),
            {SortedNodes, Gains}
    end.