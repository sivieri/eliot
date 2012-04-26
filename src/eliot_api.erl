%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Framework.
-module(eliot_api).
-include("eliot.hrl").
-export([nodeid/1, nodeaddr/1, set_node_name/1, get_node_name/0, export/1, unexport/1, put_data/2, get_data/1]).
-export([send/3, send_test/3, bcast_send/1, bcast_send/2]).
-export([spawn/2, spawn/3, spawn/4, spawn/5, bcast_spawn/1, bcast_spawn/2, bcast_spawn/3, bcast_spawn/4]).

% Public API

%% Set the name of this node.
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

%% Get the name of this node.
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

%% Get the node address given its unique ID (i.e. from "1" to "node_1").
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

%% Get the node unique ID given its address (i.e. from "node_1" to "1").
-spec(nodeid(atom()) -> integer()).
nodeid(NodeAddr) ->
    list_to_integer(string:substr(atom_to_list(NodeAddr), 6)).

%% Put some data associated to this node.
-spec(put_data(any(), any()) -> true).
-ifdef(simulation).
put_data(Key, Value) ->
    DataRef = case get(datatableref) of
        undefined ->
            ets:new(datatable, [set, public]);
        Ref ->
            Ref
    end,
    put(datatableref, DataRef),
    ets:insert(datatable, {Key, Value}).
-else.
put_data(Key, Value) ->
    DataRef = case application:get_env(eliot, datatableref) of
        undefined ->
            ets:new(datatable, [set, public]);
        Ref ->
            Ref
    end,
    application:set_env(eliot, datatableref, DataRef),
    ets:insert(datatable, {Key, Value}).
-endif.

%% Get some data previously saved in this node.
-spec(get_data(any()) -> list()).
-ifdef(simulation).
get_data(Key) ->
    case get(datatableref) of
        undefined ->
            [];
        Ref ->
            ets:lookup(Ref, Key)
    end.
-else.
get_data(Key) ->
    case application:get_env(eliot, datatableref) of
        undefined ->
            [];
        Ref ->
            ets:lookup(Ref, Key)
    end.
-endif.

%% Export a process (indicated by its name or PID), so that it can be
%% reached by the external world.
-spec(export(atom() | pid()) -> ok).
-ifdef(simulation).
export(Subject) when is_atom(Subject) ->
	eliot_simulator:export(Subject);
export(Subject) ->
    eliot_export:export_real(Subject).
-else.
export(Subject) ->
    eliot_export:export_real(Subject).
-endif.

%% Remove the given process from the list of exported processes;
%% from now on this process will not be reached from the external
%% world.
-spec(unexport(atom() | pid()) -> ok).
-ifdef(simulation).
unexport(Subject) when is_atom(Subject) ->
	eliot_export:unexport(eliot_simulator:get_simname(Subject));
unexport(Subject) ->
    eliot_export:unexport(Subject).
-else.
unexport(Subject) ->
    eliot_export:unexport(Subject).
-endif.

%% Send a message to a specific process on the given node; the
%% process must have been exported in the receiving node.
-spec(send(atom() | pid(), {atom(), node()}, any()) -> ok).
-ifdef(simulation).
send(Name, {NodeName, NodeAddr}, Msg) ->
    case utils:get_host_ip() == NodeAddr of
        true ->
            eliot_dispatcher ! {simulation, {Name, NodeName}, msg(Msg)}; % Send to simulated nodes if receiver is the same node...
        false ->
            {eliot_dispatcher, utils:join_name(?NODENAME, NodeAddr)} ! {connect, Name, msg(Msg)} % ... or send to all if receiver is different
    end,
    ok.
-else.
send(Name, {_NodeName, NodeAddr}, Msg) ->
	{eliot_dispatcher, NodeAddr} ! {connect, Name, msg(Msg)},
	ok.
-endif.

%% This send function overrides the standard send function by automatically adding
%% a standard string as the name of the node sending the message.
%% The standard function requires the node name to be registered in the process
%% dictionary, while usually an external observer wants to inject messages to
%% a node of the network; this function allows this action to be performed correctly
%% according to the framework specifications.
-spec(send_test(atom() | pid(), {atom(), node()}, any()) -> ok).
send_test(Name, {NodeName, _NodeAddr}, Msg) ->
    eliot_dispatcher ! {simulation, {Name, NodeName}, msg_test(Msg)}.

%% Send a message to all the exported processes on all the nodes reachable from the
%% current one.
-spec(bcast_send(any()) -> ok).
-ifdef(simulation).
bcast_send(Msg) ->
    eliot_dispatcher ! {simulation, all, msg(Msg)}, % Send to simulated nodes...
    rpc:abcast(nodes(), eliot_dispatcher, {connect, all, msg(Msg)}), % ... and to real ones, in case we are in mixed simulation.
    ok.
-else.
bcast_send(Msg) ->
	rpc:abcast(nodes(), eliot_dispatcher, {connect, all, msg(Msg)}),
    ok.
-endif.

%% Send a message to a specific process on all the nodes reachable from the
%% current one; the process must have been exported in the receiving nodes.
-spec(bcast_send(atom() | pid(), any()) -> ok).
-ifdef(simulation).
bcast_send(Name, Msg) ->
    eliot_dispatcher ! {simulation, Name, msg(Msg)},
    rpc:abcast(nodes(), eliot_dispatcher, {connect, Name, msg(Msg)}),
    ok.
-else.
bcast_send(Name, Msg) ->
	rpc:abcast(nodes(), eliot_dispatcher, {connect, Name, msg(Msg)}),
    ok.
-endif.

%% Spawn a process executing the given function on the given node.
-spec(spawn(node(), fun()) -> ok).
spawn(NodeAddr, Fun) ->
    {eliot_dispatcher, NodeAddr} ! {spawn, Fun},
	ok.

%% Spawn a process executing the given function on the given node.
-spec(spawn(node(), atom(), atom(), list()) -> ok).
spawn(NodeAddr, Module, Function, Args) ->
    {eliot_dispatcher, NodeAddr} ! {spawn, Module, Function, Args},
	ok.

%% Spawn a process executing the given function on the given node,
%% if the condition evaluates to true in the node itself.
-spec(spawn(node(), fun(), fun()) -> ok).
spawn(NodeAddr, Fun, Condition) ->
    {eliot_dispatcher, NodeAddr} ! {spawn, Fun, Condition},
    ok.

%% Spawn a process executing the given function on the given node,
%% if the condition evaluates to true in the node itself.
-spec(spawn(node(), atom(), atom(), list(), fun()) -> ok).
spawn(NodeAddr, Module, Function, Args, Condition) ->
    {eliot_dispatcher, NodeAddr} ! {spawn, Module, Function, Args, Condition},
    ok.

%% Spawn a process executing the given function on all the nodes
%% reachable from the current one.
-spec(bcast_spawn(fun()) -> ok).
bcast_spawn(Fun) ->
    rpc:abcast(nodes(), eliot_dispatcher, {spawn, Fun}).

%% Spawn a process executing the given function on all the nodes
%% reachable from the current one.
-spec(bcast_spawn(atom(), atom(), list()) -> ok).
bcast_spawn(Module, Function, Args) ->
    rpc:abcast(nodes(), eliot_dispatcher, {spawn, Module, Function, Args}).

%% Spawn a process executing the given function on all the nodes
%% reachable from the current one, if the condition evaluates to true in the nodes themselves.
-spec(bcast_spawn(fun(), fun()) -> ok).
bcast_spawn(Fun, Condition) ->
    rpc:abcast(nodes(), eliot_dispatcher, {spawn, Fun, Condition}).

%% Spawn a process executing the given function on all the nodes
%% reachable from the current one, if the condition evaluates to true in the nodes themselves.
-spec(bcast_spawn(atom(), atom(), list(), fun()) -> ok).
bcast_spawn(Module, Function, Args, Condition) ->
    rpc:abcast(nodes(), eliot_dispatcher, {spawn, Module, Function, Args, Condition}).

% Private API

msg(Msg) ->
    {{eliot_api:get_node_name(), utils:get_host_ip()}, Msg}.

msg_test(Msg) ->
    {{test, utils:get_host_ip()}, Msg}.
