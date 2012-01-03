%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Main Wireless Sensors Network framework and simulator.
-module(wsn_api).
-include("wsn.hrl").
-export([read_net/1, nodeid/1, nodeaddr/1, set_node_name/1, get_node_name/0]).
-export([send/3, bcast_send/1, bcast_send/2, spawn/2, spawn/4, bcast_spawn/1, bcast_spawn/3, export/1, unexport/1]).

% Public API

-spec(read_net(string()) -> net()).
read_net(Filename) ->
    {ok, Device} = file:open(Filename,[read]),
    read_net(Device, sets:new(), dict:new()).

-spec(set_node_name(atom()) -> ok).
set_node_name(NodeId) ->
    Name = nodeaddr(NodeId),
    application:set_env(wsn, name, Name),
    application:set_env(trickle, name, Name).

-spec(get_node_name() -> atom() | error).
get_node_name() ->
    case application:get_env(trickle, name) of
        {ok, Name} ->
            Name;
        undefined ->
            error
    end.

-spec(nodeaddr(integer() | string()) -> atom()).
nodeaddr(NodeId) when is_integer(NodeId) ->
    list_to_atom("node_" ++ utils:format("~p", [NodeId]));
nodeaddr(NodeId) ->
    list_to_atom("node_" ++ NodeId).

-spec(nodeid(atom()) -> integer()).
nodeid(NodeAddr) ->
    list_to_integer(string:substr(atom_to_list(NodeAddr), 6)).

-spec(export(atom() | pid()) -> ok).
export(Subject) ->
	wsn_export:export(Subject).

-spec(unexport(atom() | pid()) -> ok).
unexport(Subject) ->
	wsn_export:unexport(Subject).

-spec(send(atom() | pid(), node(), any()) -> ok).
send(Name, NodeAddr, Msg) ->
	{wsn_dispatcher, NodeAddr} ! {connect, Name, Msg},
	ok.

-spec(bcast_send(any()) -> ok).
bcast_send(Msg) ->
	rpc:abcast(nodes(), wsn_dispatcher, {connect, all, Msg}),
    ok.

-spec(bcast_send(atom() | pid(), any()) -> ok).
bcast_send(Name, Msg) ->
	rpc:abcast(nodes(), wsn_dispatcher, {connect, Name, Msg}),
    ok.

-spec(spawn(node(), fun()) -> ok).
spawn(NodeAddr, Fun) ->
	_Pid = erlang:spawn(NodeAddr, Fun),
	ok.

-spec(spawn(node(), atom(), atom(), list()) -> ok).
spawn(NodeAddr, Module, Function, Args) ->
	_Pid = erlang:spawn(NodeAddr, Module, Function, Args),
	ok.

-spec(bcast_spawn(fun()) -> ok).
bcast_spawn(Fun) ->
	lists:foreach(fun(Node) -> wsn_api:spawn(Node, Fun) end, nodes()).

-spec(bcast_spawn(atom(), atom(), list()) -> ok).
bcast_spawn(Module, Function, Args) ->
	lists:foreach(fun(Node) -> wsn_api:spawn(Node, Module, Function, Args) end, nodes()).

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
