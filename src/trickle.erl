%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @reference <a href="http://portal.acm.org/citation.cfm?id=1251177">Trickle</a>
-module(trickle).
-export([start/1, start_simulation/1, start_simulation/2, trickle/0, update_version/2]).
-define(TAU_MAX, 60000).
-define(TAU_MIN, 1000).
-define(K, 2).
-define(PAYLOAD_STRING, "Payload version ").

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
    T = utils:random(Tau),
    TRef = erlang:send_after(T, self(), transmit),
    TauRef = erlang:send_after(Tau, self(), restart),
    trickle(Tau, {TauRef, TRef}, 0, {1, string(1)}).

%% @doc Update the version of the given node.
%% @spec update_version(atom(), integer()) -> ok
-spec(update_version(atom(), integer()) -> ok).
update_version(DestId, Version) ->
    wsn:send_ignore_gain(console, DestId, {update, Version, string(Version)}),
	ok.

% Private API

%% @private
-spec(trickle(non_neg_integer(), {reference(), reference()}, non_neg_integer(), {integer(), string()}) -> none()).
trickle(Tau, {TauRef, TRef}, Counter, {Version, Payload}) ->
    receive
        transmit when Counter < ?K ->
            wsn:send(get(myid), all, {version, Version}),
            trickle(Tau, {TauRef, TRef}, Counter, {Version, Payload});
        transmit ->
            trickle(Tau, {TauRef, TRef}, Counter, {Version, Payload});
        restart ->
            NewTau = utils:update_tau(Tau, ?TAU_MIN, ?TAU_MAX),
            T = utils:random(NewTau),
            NewTRef = erlang:send_after(T, self(), transmit),
            NewTauRef = erlang:send_after(NewTau, self(), restart),
            trickle(NewTau, {NewTauRef, NewTRef}, 0, {Version, Payload});
        {_SourceId, _RSSI, {version, NewVersion}} when NewVersion < Version ->
            %io:format("~p: Received version ~p from ~p with RSSI = ~p (older)~n", [get(myid), NewVersion, SourceId, RSSI]),
            wsn:send(get(myid), all, {update, Version, Payload}),
            erlang:cancel_timer(TRef),
            trickle(Tau, {TauRef, TRef}, Counter, {Version, Payload});
        {SourceId, RSSI, {version, NewVersion}} when NewVersion > Version ->
            io:format("~p: Received version ~p from ~p with RSSI = ~p (newer)~n", [get(myid), NewVersion, SourceId, RSSI]),
            wsn:send(get(myid), all, {version, Version}),
            erlang:cancel_timer(TRef),
            erlang:cancel_timer(TauRef),
            NewTau = ?TAU_MIN,
            T = utils:random(NewTau),
            NewTRef = erlang:send_after(T, self(), transmit),
            NewTauRef = erlang:send_after(NewTau, self(), restart),
            trickle(NewTau, {NewTauRef, NewTRef}, 0, {Version, Payload});
        {_SourceId, _RSSI, {version, NewVersion}} when NewVersion == Version ->
            %io:format("~p: Received version ~p from ~p with RSSI = ~p (same)~n", [get(myid), NewVersion, SourceId, RSSI]),
            trickle(Tau, {TauRef, TRef}, Counter + 1, {Version, Payload});
        {SourceId, RSSI, {update, NewVersion, NewPayload}} when NewVersion > Version ->
            io:format("~p: Received code ~p from ~p with RSSI = ~p (newer)~n", [get(myid), NewVersion, SourceId, RSSI]),
            erlang:cancel_timer(TRef),
            erlang:cancel_timer(TauRef),
            NewTau = ?TAU_MIN,
            T = utils:random(NewTau),
            NewTRef = erlang:send_after(T, self(), transmit),
            NewTauRef = erlang:send_after(NewTau, self(), restart),
            trickle(NewTau, {NewTauRef, NewTRef}, 0, {NewVersion, NewPayload});
        {_SourceId, _RSSI, {version, _NewVersion, _NewPayload}} ->
            %io:format("~p: Received code ~p from ~p with RSSI = ~p (same or older)~n", [get(myid), NewVersion, SourceId, RSSI]),
            trickle(Tau, {TauRef, TRef}, Counter, {Version, Payload})
    end.

%% @private
-spec(string(integer()) -> string()).
string(Version) ->
    ?PAYLOAD_STRING ++ integer_to_list(Version).
