%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @reference <a href="http://portal.acm.org/citation.cfm?id=1251177">Trickle</a>
-module(eliot_trickle).
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
    erlang:export(trickle),
    {ok, Pid}.

%% @doc Implementation of the algorithm for each node.
%% @spec trickle() -> none()
-spec(trickle() -> none()).
trickle() ->
    Tau = ?TAU_MIN,
    T = random(Tau),
    TRef = erlang:send_after(T, trickle, transmit),
    TauRef = erlang:send_after(Tau, trickle, restart),
    Id = eliot_api:nodeid(eliot_api:get_node_name()),
    Msg = <<Id:?SRCADDR, 0:?VERSION, <<0>>/binary>>,
    trickle(Tau, {TauRef, TRef}, 0, Msg).

update_version(Pid, Version, Payload) when is_binary(Payload) ->
    Pid ! {local, Version, Payload};
update_version(Pid, Version, Payload) ->
    Pid ! {local, Version, term_to_binary(Payload)}.

% Private API

%% @private
-spec(trickle(non_neg_integer(), {reference(), reference()}, non_neg_integer(), binary()) -> none()).
trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>) ->
    receive
        transmit when Counter < ?K ->
            Id = eliot_api:nodeid(eliot_api:get_node_name()),
            {trickle, all} ! {version, <<Id:?SRCADDR, Version:?VERSION, Payload/binary>>},
            io:format("~p: Sending code ~p~n", [eliot_api:get_node_name(), Version]),
            trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>);
        transmit ->
            trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>);
        restart ->
            NewTau = update_tau(Tau, ?TAU_MIN, ?TAU_MAX),
            T = random(NewTau),
            NewTRef = erlang:send_after(T, trickle, transmit),
            NewTauRef = erlang:send_after(NewTau, trickle, restart),
            trickle(NewTau, {NewTauRef, NewTRef}, 0, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>);
        {local, NewVersion, NewPayload} ->
            if
                NewVersion > Version ->
                    erlang:cancel_timer(TRef),
                    erlang:cancel_timer(TauRef),
                    NewTau = ?TAU_MIN,
                    T = random(NewTau),
                    NewTRef = erlang:send_after(T, trickle, transmit),
                    NewTauRef = erlang:send_after(NewTau, trickle, restart),
                    NewSrc = eliot_api:nodeid(eliot_api:get_node_name()),
                    trickle(NewTau, {NewTauRef, NewTRef}, 0, <<NewSrc:?SRCADDR, NewVersion:?VERSION, NewPayload/binary>>);
                true ->
                    io:format("~p: Local update canceled, version ~p =< ~p~n", [eliot_api:get_node_name(), NewVersion, Version]),
                    trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>)
            end;
        {RSSI, SourceId, {version, <<NewSrc:?SRCADDR, NewVersion:?VERSION, NewPayload/binary>>}} when NewVersion > Version ->
            io:format("~p: Received version ~p from ~p (newer) with RSSI ~p~n", [eliot_api:get_node_name(), NewVersion, SourceId, RSSI]),
            erlang:cancel_timer(TRef),
            erlang:cancel_timer(TauRef),
            NewTau = ?TAU_MIN,
            T = random(NewTau),
            NewTRef = erlang:send_after(T, trickle, transmit),
            NewTauRef = erlang:send_after(NewTau, trickle, restart),
            trickle(NewTau, {NewTauRef, NewTRef}, 0, <<NewSrc:?SRCADDR, NewVersion:?VERSION, NewPayload/binary>>);
        {_RSSI, _SourceId, {version, <<_NewSrc:?SRCADDR, _NewVersion:?VERSION, _NewPayload/binary>>}} ->
            %io:format("~p: Received code ~p from ~p (same or older)~n", [eliot_api:get_node_name(), NewVersion, SourceId]),
            trickle(Tau, {TauRef, TRef}, Counter, <<Src:?SRCADDR, Version:?VERSION, Payload/binary>>);
        Any ->
            io:format("~p: Cannot parse ~p~n", [eliot_api:get_node_name(), Any]),
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
