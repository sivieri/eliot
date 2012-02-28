%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Framework.
-module(eliot_api).
-include("eliot.hrl").
-export([nodeid/1, nodeaddr/1, set_node_name/1, get_node_name/0]).
-export([send/3, bcast_send/1, bcast_send/2, spawn/2, spawn/4, bcast_spawn/1, bcast_spawn/3, export/1, unexport/1]).

% Public API

-spec(set_node_name(atom()) -> ok).
-ifdef(simulation).
set_node_name(NodeId) ->
    Name = nodeaddr(NodeId),
    put(name, Name).
-else.
set_node_name(NodeId) ->
    Name = nodeaddr(NodeId),
    application:set_env(eliot, name, Name).
-endif.

-spec(get_node_name() -> atom() | error).
-ifdef(simulation).
get_node_name() ->
    case get(name) of
        undefined ->
            error;
        Name ->
            Name
    end.
-else.
get_node_name() ->
    case application:get_env(eliot, name) of
        undefined ->
            error;
        Name ->
            Name
    end.
-endif.

-spec(nodeaddr(atom() | [atom()] | integer()) -> atom()).
nodeaddr(NodeId) when is_integer(NodeId) ->
    list_to_atom("node_" ++ utils:format("~p", [NodeId]));
nodeaddr(NodeId) when is_list(NodeId) andalso is_atom(hd(NodeId))->
    list_to_atom("node_" ++ atom_to_list(hd(NodeId)));
nodeaddr(NodeId) when is_atom(NodeId) ->
    case string:chr(atom_to_list(NodeId), $_) of
        0 ->
            list_to_atom("node_" ++ atom_to_list(NodeId));
        _ ->
            NodeId
    end;
nodeaddr(NodeId) ->
    case string:chr(NodeId, $_) of
        0 ->
            list_to_atom("node_" ++ NodeId);
        _ ->
            list_to_atom(NodeId)
    end.

-spec(nodeid(atom()) -> integer()).
nodeid(NodeAddr) ->
    list_to_integer(string:substr(atom_to_list(NodeAddr), 6)).

-spec(export(atom() | pid()) -> ok).
-ifdef(simulator).
export(Subject) when is_atom(Subject) ->
	eliot_export:export(eliot_simulator:get_simname(Subject));
export(Subject) ->
    eliot_export:export(Subject).
-else.
export(Subject) ->
    eliot_export:export(Subject).
-endif.

-spec(unexport(atom() | pid()) -> ok).
-ifdef(simulator).
unexport(Subject) when is_atom(Subject) ->
	eliot_export:unexport(eliot_simulator:get_simname(Subject));
unexport(Subject) ->
    eliot_export:unexport(Subject).
-else.
unexport(Subject) ->
    eliot_export:unexport(Subject).
-endif.

-spec(send(atom() | pid(), {atom(), node()}, any()) -> ok).
-ifdef(simulator).
send(Name, {NodeName, _NodeAddr}, Msg) ->
    eliot_dispatcher ! {simulation, {Name, NodeName}, msg(Msg)},
    ok.
-else.
send(Name, {_NodeName, NodeAddr}, Msg) ->
	{eliot_dispatcher, NodeAddr} ! {connect, Name, msg(Msg)},
	ok.
-endif.

-spec(bcast_send(any()) -> ok).
-ifdef(simulator).
bcast_send(Msg) ->
    eliot_dispatcher ! {simulation, all, msg(Msg)},
    ok.
-else.
bcast_send(Msg) ->
	rpc:abcast(nodes(), eliot_dispatcher, {connect, all, msg(Msg)}),
    ok.
-endif.

-spec(bcast_send(atom() | pid(), any()) -> ok).
-ifdef(simulator).
bcast_send(Name, Msg) ->
    eliot_dispatcher ! {simulation, Name, msg(Msg)},
    ok.
-else.
bcast_send(Name, Msg) ->
	rpc:abcast(nodes(), eliot_dispatcher, {connect, Name, msg(Msg)}),
    ok.
-endif.

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
	lists:foreach(fun(Node) -> eliot_api:spawn(Node, Fun) end, nodes()).

-spec(bcast_spawn(atom(), atom(), list()) -> ok).
bcast_spawn(Module, Function, Args) ->
	lists:foreach(fun(Node) -> eliot_api:spawn(Node, Module, Function, Args) end, nodes()).

% Private API

msg(Msg) ->
    {{eliot_api:get_node_name(), node()}, Msg}.
