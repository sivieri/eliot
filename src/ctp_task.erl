-module(ctp_task).
-include("ctp.hrl").
-export([ctp/0, collect/2]).
-define(TAU_MAX, 600000).
-define(TAU_MIN, 64).

% Public API

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

collect(Pid, Data) ->
    Pid ! {local, Data}.

% Private API

%% @private
-spec(ctp(pid(), pid(), pid()) -> none()).
ctp(RoutingPid, LinkPid, FwdPid) ->
    receive
        {local, Data} ->
            FwdPid ! {collect, Data},
            ctp(RoutingPid, LinkPid, FwdPid);
        {RSSI, {SourceId, Msg}} when is_record(Msg, ack)  ->
            io:format("~p: Received ack from ~p with RSSI = ~p~n", [eliot_api:get_node_name(), SourceId, RSSI]),
            FwdPid ! {SourceId, RSSI, Msg},
            ctp(RoutingPid, LinkPid, FwdPid);
        {RSSI, {SourceId, Msg}} when is_record(Msg, routing) ->
            %io:format("~p: Received msg ~p from ~p with RSSI = ~p~n", [eliot_api:get_node_name(), Msg, SourceId, RSSI]),
            LinkPid ! {SourceId, RSSI, Msg},
            ctp(RoutingPid, LinkPid, FwdPid);
        {RSSI, {SourceId, Msg}} when is_record(Msg, data) ->
            io:format("~p: Received msg ~p from ~p with RSSI = ~p~n", [eliot_api:get_node_name(), Msg, SourceId, RSSI]),
            FwdPid ! {SourceId, RSSI, Msg},
            ctp(RoutingPid, LinkPid, FwdPid);
        {RSSI, {SourceId, {collect, Data}}} ->
            io:format("~p: Data to be collected from ~p with RSSI = ~p~n", [eliot_api:get_node_name(), SourceId, RSSI]),
            FwdPid ! {collect, Data},
            ctp(RoutingPid, LinkPid, FwdPid);
        {RSSI, {SourceId, {collect, Data, Timeout}}} ->
            io:format("~p: Data to be collected in a while from ~p with RSSI = ~p~n", [eliot_api:get_node_name(), SourceId, RSSI]),
            FwdPid ! {collect, Data, Timeout},
            ctp(RoutingPid, LinkPid, FwdPid);
        Any ->
            io:format("~p: Received ~p~n", [eliot_api:get_node_name(), Any]),
            ctp(RoutingPid, LinkPid, FwdPid)
    end.

%% @private
-spec(root(atom()) -> 0 | 1).
root(Id) ->
    if
        Id rem 16 == 15 ->
            put(collector, true),
            1;
        true ->
            put(collector, false),
            0
    end.
