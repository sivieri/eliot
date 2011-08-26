%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Forwarding engine, as suggested by the TinyOS implementation
%% protocol.
%% @reference <a href="http://www.tinyos.net/tinyos-2.x/doc/html/tep123.html">TinyOS TEP 123</a>
-module(ctp_fwd).
-include("ctp.hrl").
-export([fwd_engine/2]).
-define(RESEND, 250).

% Public API

%% @doc Start the engine.
%% @spec fwd_engine({atom(), boolean()}, {pid(), pid()}) -> none()
-spec(fwd_engine({atom(), boolean()}, {pid(), pid()}) -> none()).
fwd_engine({NodeId, Collector}, {RoutingEngine, LinkEngine}) ->
    put(myid, NodeId),
    put(collector, Collector),
    put(routing, RoutingEngine),
    put(link, LinkEngine),
    fwd_engine(0, queue:new(), []).

% Private API

%% @private
-spec(fwd_engine(non_neg_integer(), queue(), [#data{}]) -> none()).
fwd_engine(DataCounter, FwdList, SentList) ->
    receive
        {resend, SourceId, Block} ->
            RoutingPid = get(routing),
            % Re-send a packet if the ack has not been received
            RoutingPid ! {self(), update_ack, SourceId},
            NewSentList = lists:filter(fun(Element) when Element == Block ->
                                               Msg = Block#cache.msg,
                                               self() ! {collect, Msg#data.payload, Block#cache.timeout},
                                               false;
                                          (_Element) ->
                                               true end, SentList),
            fwd_engine(DataCounter, FwdList, NewSentList);
        {_SourceId, _RSSI, Msg} when is_record(Msg, ack)  ->
            NewSentList = lists:filter(fun(Block) ->
                                               Msg2 = Block#cache.msg,
                                               case Msg2 of 
                                                   Msg2 when Msg2#data.seqno == Msg#ack.id ->
                                                       false;
                                                   _Msg2 ->
                                                       true
                                               end end, SentList),
            fwd_engine(DataCounter, FwdList, NewSentList);
        {SourceId, _RSSI, Msg} when is_record(Msg, data) ->
            RoutingPid = get(routing),
            LinkPid = get(link),
            AckMsg = #ack{id = Msg#data.seqno},
            wsn:send(get(myid), SourceId, AckMsg),
            % Check ETX
            {_, Etx} = utils:rpc(RoutingPid, lower),
            if
                Etx >= Msg#data.etx ->
                    LinkPid ! {transmit, cancel};
                true ->
                    ok
            end,
            % Start beaconing soon after this if there is a pull request
            case Msg#data.pull of
                true ->
                    LinkPid ! {transmit, zero};
                false ->
                    ok
            end,
            % Do not forward if I am a collector
            case get(collector) of
                true ->
                    io:format("~p: Collected data ~p~n", [get(myid), Msg#data.payload]);
                false ->
                    % Do not forward if I have already forwarded the package
                    case queue:member(Msg, FwdList) of
                        true ->
                            ok;
                        false ->
                            self() ! {collect, Msg#data.payload},
                            NewFwdList = add_to_queue(Msg, FwdList),
                            fwd_engine(DataCounter, NewFwdList, SentList)
                    end
            end,
            fwd_engine(DataCounter, FwdList, SentList);
        {collect, Data} ->
            self() ! {collect, Data, ?RESEND},
            fwd_engine(DataCounter, FwdList, SentList);
        {collect, Data, Timeout} ->
            RoutingPid = get(routing),
            case get(collector) of
                true ->
                    io:format("~p: Received data directly = ~p~n", [get(myid), Data]),
                    fwd_engine(DataCounter, FwdList, SentList);
                false ->
                    NewTimeout = Timeout * 2,
                    {Parent, Etx} = utils:rpc(RoutingPid, lower),
                    Msg = #data{etx = Etx, payload = Data, seqno = DataCounter},
                    io:format("~p: Routing data to ~p~n", [get(myid), Parent]),
                    wsn:send(get(myid), Parent, Msg),
                    Block = #cache{msg = Msg, timeout = NewTimeout},
                    erlang:send_after(NewTimeout, self(), {resend, Parent, Block}),
                    fwd_engine(DataCounter + 1, FwdList, [Block|SentList])
            end;
        Any ->
            io:format("Fwd engine: received ~p~n", [Any]),
            fwd_engine(DataCounter, FwdList, SentList)
    end.

%% @private
-spec(add_to_queue(#data{}, queue()) -> queue()).
add_to_queue(Element, Queue) ->
    case queue:len(Queue) of
        10 ->
            {_, Q1} = queue:out(Queue),
            queue:in(Element, Q1);
        _ ->
            queue:in(Element, Queue)
    end.
