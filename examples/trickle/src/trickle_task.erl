%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @reference <a href="http://portal.acm.org/citation.cfm?id=1251177">Trickle</a>
-module(trickle_task).
-export([start_link/0, trickle/0, update_version/3]).
-define(TAU_MAX, 60000).
-define(TAU_MIN, 1000).
-define(K, 2).
-define(SRCADDR, 16/unsigned-little-integer).
-define(VERSION, 32/unsigned-little-integer).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, trickle, []),
    register(trickle, Pid),
    wsn_export:export(trickle),
    {ok, Pid}.

%% @doc Implementation of the algorithm for each node.
%% @spec trickle() -> none()
-spec(trickle() -> none()).
trickle() ->
    Tau = ?TAU_MIN,
    T = random(Tau),
    TRef = erlang:send_after(T, trickle, transmit),
    TauRef = erlang:send_after(Tau, trickle, restart),
    Id = wsn_api:nodeid(wsn_api:get_node_name()),
    Msg = <<Id:?SRCADDR, 0:?VERSION, <<0>>/binary>>,
    trickle(Tau, {TauRef, TRef}, 0, Msg).

%% @doc Update the version of the given node.
%% @spec update_version(atom(), integer(), any()) -> ok
-spec(update_version(atom(), integer(), any()) -> ok).
update_version(DestId, Version, Payload) ->
    Id = wsn_api:nodeid(DestId),
    PayloadB = term_to_binary(Payload),
    Msg = <<Id:?SRCADDR, Version:?VERSION, PayloadB/binary>>,
    wsn_api:send(trickle, node(), Msg),
    ok.

% Private API

%% @private
-spec(trickle(non_neg_integer(), {reference(), reference()}, non_neg_integer(), binary()) -> none()).
trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>) ->
    receive
        transmit when Counter < ?K ->
            Id = wsn_api:nodeid(wsn_api:get_node_name()),
            wsn_api:bcast_send(trickle, {version, <<Id:?SRCADDR, Version:?VERSION, Payload/binary>>}),
            io:format("~p: Sending code ~p~n", [wsn_api:get_node_name(), Version]),
            trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>);
        transmit ->
            trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>);
        restart ->
            NewTau = update_tau(Tau, ?TAU_MIN, ?TAU_MAX),
            T = random(NewTau),
            NewTRef = erlang:send_after(T, trickle, transmit),
            NewTauRef = erlang:send_after(NewTau, trickle, restart),
            trickle(NewTau, {NewTauRef, NewTRef}, 0, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>);
        {SourceId, {version, <<NewSrc:?SRCADDR, NewVersion:?VERSION, NewPayload/binary>>}} when NewVersion > Version ->
            io:format("~p: Received version ~p from ~p (newer)~n", [wsn_api:get_node_name(), NewVersion, SourceId]),
            erlang:cancel_timer(TRef),
            erlang:cancel_timer(TauRef),
            NewTau = ?TAU_MIN,
            T = random(NewTau),
            NewTRef = erlang:send_after(T, trickle, transmit),
            NewTauRef = erlang:send_after(NewTau, trickle, restart),
            trickle(NewTau, {NewTauRef, NewTRef}, 0, <<NewSrc:?SRCADDR, NewVersion:?VERSION, NewPayload/binary>>);
        {SourceId, {version, <<_NewSrc:?SRCADDR, NewVersion:?VERSION, _NewPayload/binary>>}} ->
            io:format("~p: Received code ~p from ~p (same or older)~n", [wsn_api:get_node_name(), NewVersion, SourceId]),
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
