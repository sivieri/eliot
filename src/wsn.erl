%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Main Wireless Sensors Network framework and simulator.
-module(wsn).
-include("wsn.hrl").
-export([read_net/1, nodeid/1, nodeaddr/1]).
-export([myaddr/0, register/2, send/3, bcast/2, spawn/2, spawn/4, bcast_spawn/1, bcast_spawn/3]).

% Public API

-spec(read_net(string()) -> net()).
read_net(Filename) ->
    {ok, Device} = file:open(Filename,[read]),
    read_net(Device, sets:new(), dict:new()).

-spec(nodeid(integer() | string()) -> atom()).
nodeid(NodeAddr) when is_integer(NodeAddr) ->
    list_to_atom("mote_" ++ utils:format("~p", [NodeAddr]));
nodeid(NodeAddr) ->
    list_to_atom("mote_" ++ NodeAddr).

-spec(nodeaddr(atom()) -> integer()).
nodeaddr(NodeId) ->
    list_to_integer(string:substr(atom_to_list(NodeId), 6)).

myaddr() ->
	erlang:node().

register(Name, Pid) ->
	erlang:register(Name, Pid).

send(Name, NodeAddr, Msg) ->
	{Name, NodeAddr} ! Msg.

bcast(Name, Msg) ->
	rpc:abcast(nodes(), Name, Msg).

spawn(NodeAddr, Fun) ->
	erlang:spawn(NodeAddr, Fun).

spawn(NodeAddr, Module, Function, Args) ->
	erlang:spawn(NodeAddr, Module, Function, Args).

bcast_spawn(Fun) ->
	lists:foreach(fun(Node) -> erlang:spawn(Node, Fun) end, nodes()).

bcast_spawn(Module, Function, Args) ->
	lists:foreach(fun(Node) -> erlang:spawn(Node, Module, Function, Args) end, nodes()).

% Private API

-spec(read_net(pid(), set(), dict()) -> net()).
read_net(Device, Nodes, Gains) ->
    case io:get_line(Device, "") of
	"gain"++Rest ->
	    [Node1, Node2, Gain] = string:tokens(Rest," \t"),
	    {G,_}=string:to_float(Gain), % remove trailing CR and LF
	    NewNodes = sets:add_element(nodeid(Node1), Nodes),
	    NewGains = dict:store({nodeid(Node1), nodeid(Node2)}, G, Gains),
	    read_net(Device, NewNodes, NewGains);
	_Else ->
	    file:close(Device),
	    SortedNodes = lists:sort(sets:to_list(Nodes)),
	    {SortedNodes, Gains}
    end.
