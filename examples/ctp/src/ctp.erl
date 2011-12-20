%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @reference <a href="http://portal.acm.org/citation.cfm?id=1644040">Collection Tree Protocol</a>
-module(ctp).
-include("ctp.hrl").
-export([start/1, start_simulation/1, start_simulation/2, ctp/0, collect/2]).
-define(TAU_MAX, 600000).
-define(TAU_MIN, 64).

% Public API

%% @doc Start a real network.
%% @spec start([{atom(), integer()}]) -> ok
-spec(start([{atom(), integer()}]) -> ok).
start(Hosts) ->
    wsn:spawn_net(Hosts, ?MODULE, ctp).

%% @doc Start the simulation with the given topology.
%% @spec start_simulation(string()) -> ok
-spec(start_simulation(string()) -> ok).
start_simulation(Filename) ->
    Net = wsn:read_net(Filename),
    wsn:spawn_net(Net, [], ?MODULE, ctp).

%% @doc Start the simulation with the given topology on the given hosts.
%% @spec start_simulation(string(), [{atom(), integer()}]) -> ok
-spec(start_simulation(string(), [{atom(), integer()}]) -> ok).
start_simulation(Filename, Hosts) ->
    Net = wsn:read_net(Filename),
    wsn:spawn_net(Net, Hosts, ?MODULE, ctp).

%% @doc Implementation of the algorithm for each node.
%% @spec ctp() -> none()
-spec(ctp() -> none()).
ctp() ->
    NodeId = get(myid),
    Index = root(NodeId),
    Collector = get(collector),
    RoutingPid = spawn(fun() -> ctp_routing:routing_engine(NodeId) end),
    LinkPid = spawn(fun() -> ctp_link:link_engine({NodeId, Collector}, {RoutingPid, Index}) end),
    FwdPid = spawn(fun() -> ctp_fwd:fwd_engine({NodeId, Collector}, {RoutingPid, LinkPid}) end),
    ctp(RoutingPid, LinkPid, FwdPid).

%% @doc Send a data from the specified node; this data should be
%% routed to a collector.
%% @spec collect(atom(), any()) -> ok
-spec(collect(atom(), any()) -> ok).
collect(Node, Data) ->
    wsn:send_direct(get(myid), Node, {collect, Data}),
	ok.

% Private API

%% @private
-spec(ctp(pid(), pid(), pid()) -> none()).
ctp(RoutingPid, LinkPid, FwdPid) ->
    receive
        {SourceId, RSSI, Msg} when is_record(Msg, ack)  ->
            io:format("~p: Received ack from ~p with RSSI = ~p~n", [get(myid), SourceId, RSSI]),
            FwdPid ! {SourceId, RSSI, Msg},
            ctp(RoutingPid, LinkPid, FwdPid);
        {SourceId, RSSI, Msg} when is_record(Msg, routing) ->
            %io:format("~p: Received msg ~p from ~p with RSSI = ~p~n", [get(myid), Msg, SourceId, RSSI]),
            LinkPid ! {SourceId, RSSI, Msg},
            ctp(RoutingPid, LinkPid, FwdPid);
        {SourceId, RSSI, Msg} when is_record(Msg, data) ->
            io:format("~p: Received msg ~p from ~p with RSSI = ~p~n", [get(myid), Msg, SourceId, RSSI]),
            FwdPid ! {SourceId, RSSI, Msg},
            ctp(RoutingPid, LinkPid, FwdPid);
        {_SourceId, _RSSI, {collect, Data}} ->
            FwdPid ! {collect, Data},
            ctp(RoutingPid, LinkPid, FwdPid);
        {_SourceId, _RSSI, {collect, Data, Timeout}} ->
            FwdPid ! {collect, Data, Timeout},
            ctp(RoutingPid, LinkPid, FwdPid);
        Any ->
            io:format("~p: Received ~p~n", [get(myid), Any]),
            ctp(RoutingPid, LinkPid, FwdPid)
    end.

%% @private
-spec(root(atom()) -> 0 | 1).
root(Id) ->
    N = wsn:moteaddr(Id),
    if
        N rem 16 == 15 ->
            put(collector, true),
            1;
        true ->
            put(collector, false),
            0
    end.