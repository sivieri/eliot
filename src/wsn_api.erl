%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Framework.
-module(wsn_api).
-include("wsn.hrl").
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
    application:set_env(wsn, name, Name).
-endif.

-spec(get_node_name() -> atom() | error).
-ifdef(simulation).
get_node_name() ->
    case get(name) of
        {ok, Name} ->
            Name;
        undefined ->
            error
    end.
-else.
get_node_name() ->
    case application:get_env(wsn, name) of
        {ok, Name} ->
            Name;
        undefined ->
            error
    end.
-endif.

-spec(nodeaddr(atom() | [atom()] | integer()) -> atom()).
nodeaddr(NodeId) when is_integer(NodeId) ->
    list_to_atom("node_" ++ utils:format("~p", [NodeId]));
nodeaddr(NodeId) when is_list(NodeId) ->
    list_to_atom("node_" ++ atom_to_list(hd(NodeId)));
nodeaddr(NodeId) ->
    list_to_atom("node_" ++ atom_to_list(NodeId)).

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
	{wsn_dispatcher, NodeAddr} ! {connect, Name, msg(Msg)},
	ok.

-spec(bcast_send(any()) -> ok).
bcast_send(Msg) ->
	rpc:abcast(nodes(), wsn_dispatcher, {connect, all, msg(Msg)}),
    ok.

-spec(bcast_send(atom() | pid(), any()) -> ok).
bcast_send(Name, Msg) ->
	rpc:abcast(nodes(), wsn_dispatcher, {connect, Name, msg(Msg)}),
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

msg(Msg) ->
    {wsn_api:get_node_name(), Msg}.
