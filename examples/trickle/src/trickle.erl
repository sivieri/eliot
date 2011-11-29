%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @reference <a href="http://portal.acm.org/citation.cfm?id=1251177">Trickle</a>
-module(trickle).
-export([start/1, start_simulation/1, start_simulation/2, trickle/0, update_version/3]).
-define(TAU_MAX, 60000).
-define(TAU_MIN, 1000).
-define(K, 2).
-define(SRCADDR, 16/unsigned-little-integer).
-define(VERSION, 32/unsigned-little-integer).

% Public API

%% @doc Start a real network.
%% @spec start([{atom(), integer()}]) -> ok
-spec(start([{atom(), integer()}]) -> ok).
start(Hosts) ->
    wsn:spawn_net(Hosts, ?MODULE, trickle).

%% @doc Start the simulation with the given topology.
%% @spec start_simulation(string()) -> ok
-spec(start_simulation(string()) -> ok).
start_simulation(Filename) ->
    Net = wsn:read_net(Filename),
    wsn:spawn_net(Net, [], ?MODULE, trickle).

%% @doc Start the simulation with the given topology, starting each
%% process on a new node on the specified hosts.
%% @spec start_simulation(string(), [{atom(), integer()}]) -> ok
-spec(start_simulation(string(), [{atom(), integer()}]) -> ok).
start_simulation(Filename, Hosts) ->
    Net = wsn:read_net(Filename),
    wsn:spawn_net(Net, Hosts, ?MODULE, trickle).

%% @doc Implementation of the algorithm for each node.
%% @spec trickle() -> none()
-spec(trickle() -> none()).
trickle() ->
    Tau = ?TAU_MIN,
    T = random(Tau),
    TRef = erlang:send_after(T, self(), transmit),
    TauRef = erlang:send_after(Tau, self(), restart),
    Id = get(myaddr),
    Msg = <<Id:?SRCADDR, 0:?VERSION, <<0>>/binary>>,
    trickle(Tau, {TauRef, TRef}, 0, Msg).

%% @doc Update the version of the given node.
%% @spec update_version(atom(), integer(), any()) -> ok
-spec(update_version(atom(), integer(), any()) -> ok).
update_version(DestId, Version, Payload) ->
    Id = wsn:moteaddr(DestId),
    PayloadB = term_to_binary(Payload),
    Msg = <<Id:?SRCADDR, Version:?VERSION, PayloadB/binary>>,
    wsn:send_direct(console, DestId, {version, Msg}),
	ok.

% Private API

%% @private
-spec(trickle(non_neg_integer(), {reference(), reference()}, non_neg_integer(), binary()) -> none()).
trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>) ->
    receive
        transmit when Counter < ?K ->
            Id = get(myaddr),
            wsn:send(get(myid), all, {version, <<Id:?SRCADDR, Version:?VERSION, Payload/binary>>}),
            trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>);
        transmit ->
            trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>);
        restart ->
            NewTau = update_tau(Tau, ?TAU_MIN, ?TAU_MAX),
            T = random(NewTau),
            NewTRef = erlang:send_after(T, self(), transmit),
            NewTauRef = erlang:send_after(NewTau, self(), restart),
            trickle(NewTau, {NewTauRef, NewTRef}, 0, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>);
        {SourceId, RSSI, {version, <<NewSrc:?SRCADDR, NewVersion:?VERSION, NewPayload/binary>>}} when NewVersion > Version ->
            io:format("~p: Received version ~p from ~p with RSSI = ~p (newer)~n", [get(myid), NewVersion, SourceId, RSSI]),
            erlang:cancel_timer(TRef),
            erlang:cancel_timer(TauRef),
            NewTau = ?TAU_MIN,
            T = random(NewTau),
            NewTRef = erlang:send_after(T, self(), transmit),
            NewTauRef = erlang:send_after(NewTau, self(), restart),
            trickle(NewTau, {NewTauRef, NewTRef}, 0, <<NewSrc:?SRCADDR, NewVersion:?VERSION, NewPayload/binary>>);
        {_SourceId, _RSSI, {version, <<_NewSrc:?SRCADDR, _NewVersion:?VERSION, _NewPayload/binary>>}} ->
            %io:format("~p: Received code ~p from ~p with RSSI = ~p (same or older)~n", [get(myid), NewVersion, SourceId, RSSI]),
            trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>)
    end.

%% @private
-spec(update_tau(OldTau::integer(), TauMin::integer(), TauMax::integer()) -> integer()).
update_tau(OldTau, TauMin, _) when OldTau == 0 ->
    TauMin;
update_tau(OldTau, _, TauMax) when OldTau*2 =< TauMax ->
    OldTau*2;
update_tau(_, _, TauMax) ->
    TauMax.

%% @private
-spec(random(Tau::integer()) -> integer()).
random(Tau) ->
    N = random:uniform(),
    erlang:round(Tau/2 + erlang:round(N*Tau/2)).
