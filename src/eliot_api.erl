%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Framework.
-module(eliot_api).
-include("eliot.hrl").
-export([nodeid/1, nodeaddr/1, set_node_name/1, get_node_name/0, put_data/2, get_data/1, rpc/2, rpc_noacks/2, lpc/2, id/0, ip_to_node/1, node_to_ip/1]).
-export([send_test/3]).
-export([spawn/2, spawn/3, spawn/4, spawn/5, bcast_spawn/1, bcast_spawn/2, bcast_spawn/3, bcast_spawn/4]).

% Public API

id() ->
    nodeid(get_node_name()).

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
        {ok, Name} ->
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

%% This send function overrides the standard send function by automatically adding
%% a standard string as the name of the node sending the message.
%% The standard function requires the node name to be registered in the process
%% dictionary, while usually an external observer wants to inject messages to
%% a node of the network; this function allows this action to be performed correctly
%% according to the framework specifications.
-spec(send_test(atom() | pid(), {atom(), node()}, any()) -> ok).
send_test(Name, {NodeName, _NodeAddr}, Msg) ->
    dispatcher ! {simulation, {eliot_simulator:get_simname(Name, NodeName), NodeName}, msg_test(Msg)}.

%% Spawn a process executing the given function on the given node.
-spec(spawn(node(), fun()) -> ok).
spawn(NodeAddr, Fun) ->
    {dispatcher, NodeAddr} ! {spawn, Fun},
	ok.

%% Spawn a process executing the given function on the given node.
-spec(spawn(node(), atom(), atom(), list()) -> ok).
spawn(NodeAddr, Module, Function, Args) ->
    {dispatcher, NodeAddr} ! {spawn, Module, Function, Args},
	ok.

%% Spawn a process executing the given function on the given node,
%% if the condition evaluates to true in the node itself.
-spec(spawn(node(), fun(), fun()) -> ok).
spawn(NodeAddr, Fun, Condition) ->
    {dispatcher, NodeAddr} ! {spawn, Fun, Condition},
    ok.

%% Spawn a process executing the given function on the given node,
%% if the condition evaluates to true in the node itself.
-spec(spawn(node(), atom(), atom(), list(), fun()) -> ok).
spawn(NodeAddr, Module, Function, Args, Condition) ->
    {dispatcher, NodeAddr} ! {spawn, Module, Function, Args, Condition},
    ok.

%% Spawn a process executing the given function on all the nodes
%% reachable from the current one.
-spec(bcast_spawn(fun()) -> ok).
bcast_spawn(Fun) ->
    all ! {spawn, Fun}.

%% Spawn a process executing the given function on all the nodes
%% reachable from the current one.
-spec(bcast_spawn(atom(), atom(), list()) -> ok).
bcast_spawn(Module, Function, Args) ->
    all ! {spawn, Module, Function, Args}.

%% Spawn a process executing the given function on all the nodes
%% reachable from the current one, if the condition evaluates to true in the nodes themselves.
-spec(bcast_spawn(fun(), fun()) -> ok).
bcast_spawn(Fun, Condition) ->
    all ! {spawn, Fun, Condition}.

%% Spawn a process executing the given function on all the nodes
%% reachable from the current one, if the condition evaluates to true in the nodes themselves.
-spec(bcast_spawn(atom(), atom(), list(), fun()) -> ok).
bcast_spawn(Module, Function, Args, Condition) ->
    all ! {spawn, Module, Function, Args, Condition}.

rpc(Dest, Message) when node(Dest) == node() ->
    lpc(Dest, Message);
rpc(Dest, Message) ->
    Dest ! term_to_binary(Message),
    receive
        {_RSSI, _Source, Content} ->
            binary_to_term(Content)
    end.

rpc_noacks(Dest, Message) when node(Dest) == node() ->
    lpc(Dest, Message);
rpc_noacks(Dest, Message) when is_binary(Message) ->
    Dest ~ Message,
    receive
        {_RSSI, _Source, Content} ->
            Content
    after
        ?RPC_NOACKS ->
            {error, no_answer}
    end;
rpc_noacks(Dest, Message) ->
    Dest ~ erlang:term_to_binary(Message),
    receive
        {_RSSI, _Source, Content} ->
            erlang:binary_to_term(Content)
    after
        ?RPC_NOACKS ->
            {error, no_answer}
    end.

lpc(Dest, Message) ->
    Dest ! {self(), Message},
    receive
        {_Dest, Answer} ->
            Answer
    end.

ip_to_node({_A, _B, _C, _D} = IP) ->
    erlang:list_to_atom(?NODENAME ++ "@" ++ inet_parse:ntoa(IP)).

node_to_ip(Name) ->
    {?NODENAME, IP} = string:tokens(erlang:atom_to_list(Name), "@"),
    inet_parse:address(IP).

% Private API

msg_test(Msg) ->
    {{test, utils:get_host_ip()}, Msg}.
