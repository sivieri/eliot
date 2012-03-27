-module(ctp_task).
-include("ctp.hrl").
-export([start_link/0, ctp/0, collect/1]).
-define(TAU_MAX, 600000).
-define(TAU_MIN, 64).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, ctp, []),
    register(ctp, Pid),
    eliot_export:export(ctp),
    {ok, Pid}.

%% @doc Implementation of the algorithm for each node.
%% @spec ctp() -> none()
-spec(ctp() -> none()).
ctp() ->
    NodeId = eliot_api:nodeid(eliot_api:get_node_name()),
    Index = root(NodeId),
    Collector = get(collector),
    RoutingPid = spawn(fun() -> ctp_routing:routing_engine(NodeId) end),
    LinkPid = spawn(fun() -> ctp_link:link_engine({NodeId, Collector}, {RoutingPid, Index}) end),
    FwdPid = spawn(fun() -> ctp_fwd:fwd_engine({NodeId, Collector}, {RoutingPid, LinkPid}) end),
    ctp(RoutingPid, LinkPid, FwdPid).

%% @doc Send a data from the specified node; this data should be
%% routed to a collector.
%% @spec collect(atom(), any()) -> ok
-spec(collect(any()) -> ok).
collect(Data) ->
    eliot_api:send(ctp, node(), {collect, Data}),
    ok.

% Private API

%% @private
-spec(ctp(pid(), pid(), pid()) -> none()).
ctp(RoutingPid, LinkPid, FwdPid) ->
    receive
        {SourceId, RSSI, Msg} when is_record(Msg, ack)  ->
            io:format("~p: Received ack from ~p with RSSI = ~p~n", [eliot_api:get_node_name(), SourceId, RSSI]),
            FwdPid ! {SourceId, RSSI, Msg},
            ctp(RoutingPid, LinkPid, FwdPid);
        {SourceId, RSSI, Msg} when is_record(Msg, routing) ->
            %io:format("~p: Received msg ~p from ~p with RSSI = ~p~n", [eliot_api:get_node_name(), Msg, SourceId, RSSI]),
            LinkPid ! {SourceId, RSSI, Msg},
            ctp(RoutingPid, LinkPid, FwdPid);
        {SourceId, RSSI, Msg} when is_record(Msg, data) ->
            io:format("~p: Received msg ~p from ~p with RSSI = ~p~n", [eliot_api:get_node_name(), Msg, SourceId, RSSI]),
            FwdPid ! {SourceId, RSSI, Msg},
            ctp(RoutingPid, LinkPid, FwdPid);
        {_SourceId, _RSSI, {collect, Data}} ->
            FwdPid ! {collect, Data},
            ctp(RoutingPid, LinkPid, FwdPid);
        {_SourceId, _RSSI, {collect, Data, Timeout}} ->
            FwdPid ! {collect, Data, Timeout},
            ctp(RoutingPid, LinkPid, FwdPid);
        Any ->
            io:format("~p: Received ~p~n", [eliot_api:get_node_name(), Any]),
            ctp(RoutingPid, LinkPid, FwdPid)
    end.

%% @private
-spec(root(atom()) -> 0 | 1).
root(Id) ->
    N = eliot_api:nodeid(Id),
    if
        N rem 16 == 15 ->
            put(collector, true),
            1;
        true ->
            put(collector, false),
            0
    end.