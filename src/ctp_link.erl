%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Link estimation engine, as suggested by the TinyOS implementation
%% protocol.
%% @reference <a href="http://www.tinyos.net/tinyos-2.x/doc/html/tep123.html">TinyOS TEP 123</a>
-module(ctp_link).
-include("ctp.hrl").
-export([link_engine/2]).
-define(TAU_MAX, 600000).
-define(TAU_MIN, 64).

% Public API

%% @doc Start the engine.
%% @spec link_engine({atom(), boolean()}, {pid(), non_neg_integer()}) -> none()
-spec(link_engine({atom(), boolean()}, {pid(), non_neg_integer()}) -> none()).
link_engine({NodeId, Collector}, {RoutingEngine, Counter}) ->
    put(myid, NodeId),
    put(collector, Collector),
    put(routing, RoutingEngine),
    if
        Counter == 1 ->
            Msg = #routing{pull = true},
            wsn:send(get(myid), all, Msg);
        true ->
            ok
    end,
    Tau = ?TAU_MIN,
    T = random(Tau),
    TRef = erlang:send_after(T, self(), transmit),
    erlang:send_after(Tau, self(), restart),
    link_engine(Tau, TRef, Counter).

% Private API

%% @private
-spec(link_engine(non_neg_integer(), reference(), non_neg_integer()) -> none()).
link_engine(Tau, TRef, BeaconCounter) ->
    receive
        transmit ->
            RoutingPid = get(routing),
            % If I am a collector, then I have to send my messages regardless of my
            % neighbor table, which stays empty
            case get(collector) of
                false ->
                    case wsn:lpc(RoutingPid, lower) of
                        {_, 1000} ->
                            ok;
                        {Parent, Etx} ->
                            NewMsg = #routing{parent = Parent, etx = Etx, seqno = BeaconCounter},
                            wsn:send(get(myid), all, NewMsg)
                    end;
                true ->
                    NewMsg = #routing{seqno = BeaconCounter},
                    wsn:send(get(myid), all, NewMsg)
            end,
            link_engine(Tau, TRef, BeaconCounter + 1);
         {transmit, cancel} ->
             erlang:cancel_timer(TRef),
             self() ! transmit,
             link_engine(Tau, TRef, BeaconCounter);
        {transmit, zero} ->
            erlang:cancel_timer(TRef),
             self() ! transmit,
             link_engine(0, TRef, BeaconCounter);
        restart ->
            NewTau = update_tau(Tau, ?TAU_MIN, ?TAU_MAX),
            T = random(NewTau),
            NewTRef = erlang:send_after(T, self(), transmit),
            erlang:send_after(NewTau, self(), restart),
            link_engine(NewTau, NewTRef, BeaconCounter);
        {SourceId, _RSSI, Msg} ->
            RoutingPid = get(routing),
            % Transmit messages if there is a pull request
            case Msg#routing.pull of
                true ->
                    erlang:cancel_timer(TRef),
                    self() ! transmit,
                    NewTau = 0;
                false ->
                    NewTau = Tau
            end,
            % Ignoring answers if I am a collector
            case get(collector) of
                true ->
                    link_engine(NewTau, TRef, BeaconCounter);
                false ->
                    %io:format("~p: Received msg ~p from ~p with RSSI = ~p~n", [get(myid), Msg, SourceId, RSSI]),
                    RoutingPid ! {self(), update_routing, SourceId, Msg},
                    link_engine(NewTau, TRef, BeaconCounter)
            end;
        Any ->
            io:format("Link engine: received ~p~n", [Any]),
            link_engine(Tau, TRef, BeaconCounter)
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
