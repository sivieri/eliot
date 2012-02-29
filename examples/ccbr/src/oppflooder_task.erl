%% @author Gianpaolo Cugola <cugola@elet.polimi.it>
%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
-module(oppflooder_task).
-export([start_link/0, oppflooder/0]).
-define(MAX_WAITING_OF_MSG, 3).
-define(MAX_RECEIVED_MSG, 30).
-define(SRCADDR, 16/unsigned-little-integer).
-define(SEQNUM, 8/unsigned-little-integer).
-define(TTL, 8/unsigned-little-integer).
-define(INITIAL_OF_TTL, 20).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, oppflooder, []),
    register(oppflooder, Pid),
    eliot_export:export(oppflooder),
    {ok, Pid}.

%% @doc Send a message from the given node.
%% @spec send(atom(), any()) -> ok
-spec(send(atom(), any()) -> ok).
send(NodeId, Payload) ->
    eliot_api:send(oppflooder, node(), {send, Payload}),
    ok.

%% @doc The flood implementation for the single node.
%% @spec flood() -> none()
-spec(oppflooder() -> none()).
oppflooder() ->
    oppflooder([], dict:new(), 0).

% Private API

%% @private
-spec(oppflooder([{integer(), integer()}], dict(), integer()) -> none()).
oppflooder(ReceivedMsgs, WaitingMsgs, NextMsgNum) ->
    receive
    {RSSI, {_SourceId, {send, Payload}}} ->
        PayloadB = term_to_binary(Payload),
        Id = eliot_api:nodeid(eliot_api:get_node_name()),
        Msg = <<Id:?SRCADDR, NextMsgNum:?SEQNUM, ?INITIAL_OF_TTL:?TTL, PayloadB/binary>>,
        eliot_api:bcast_send(oppflooder, Msg),
        oppflooder(record_received({Id, NextMsgNum}, ReceivedMsgs), WaitingMsgs, (NextMsgNum + 1) rem 256);
    {Src, Seq} ->
        io:format("~p: Timer expired for message (~p, ~p) sending it~n", [get(myid), Src, Seq]),
        Msg = get_waiting(Src, Seq, WaitingMsgs),
        eliot_api:bcast_send(oppflooder, Msg),
        oppflooder(ReceivedMsgs, remove_waiting(Src, Seq, WaitingMsgs), NextMsgNum);
    {RSSI, {SourceId,  <<Src:?SRCADDR, Seq:?SEQNUM, TTL:?TTL, Payload/binary>>}} when TTL > 1 ->
        io:format("~p: Received message from ~p with RSSI=~p~n", [get(myid), SourceId, RSSI]),
        case find_waiting(Src, Seq, WaitingMsgs) of
            error ->
               case already_received({Src, Seq}, ReceivedMsgs) of
                  true ->
                      io:format("~p: Already received this msg~n", [get(myid)]),
                      oppflooder(ReceivedMsgs, WaitingMsgs, NextMsgNum);
                  false ->
                      Delay = 1000 + RSSI * 10 + random:uniform(100),
                      io:format("~p: forwarding msg in ~p ms~n", [get(myid), Delay]),
                      TRef = erlang:send_after(Delay, self(), {Src, Seq}),
                      NewTTL = TTL + 1,
                      NewMsg = <<Src:?SRCADDR, Seq:?SEQNUM, NewTTL:?TTL, Payload/binary>>,
                      oppflooder(record_received({Src, Seq}, ReceivedMsgs), add_waiting(NewMsg, TRef, WaitingMsgs, dict:size(WaitingMsgs)), NextMsgNum)
               end;
            TRef ->
                io:format("~p: Message (~p, ~p) already in queue, canceling timer~n", [get(myid), Src, Seq]),
                erlang:cancel_timer(TRef),
                oppflooder(ReceivedMsgs, remove_waiting(Src, Seq, WaitingMsgs), NextMsgNum)
        end;
    {RSSI, {SourceId, <<_Src:?SRCADDR, _Seq:?SEQNUM, _TTL:?TTL, _Payload/binary>>}} -> % TTL finished
        io:format("~p: Received message from ~p with RSSI=~p~n", [get(myid), SourceId, RSSI]),
        oppflooder(ReceivedMsgs, WaitingMsgs, NextMsgNum)
    end.

%% @private
-spec(add_waiting(binary(), reference(), dict(), integer()) -> dict()).
add_waiting(MsgId, TRef, WaitingMsgs, QueueLength) when QueueLength < ?MAX_WAITING_OF_MSG ->
    dict:store(MsgId, TRef, WaitingMsgs);
add_waiting(MsgId, TRef, WaitingMsgs, _QueueLength) ->
    Instants = dict:fold(fun(Id, Timer, AccIn) ->
                                 [{Id, erlang:read_timer(Timer)}|AccIn]
                                 end, [], WaitingMsgs),
    [{FirstId, _FirstT}|_] = lists:sort(fun({_Id1, T1}, {_Id2, T2}) ->
                                         T1 =< T2
                                         end, Instants),
    erlang:cancel_timer(dict:fetch(FirstId, WaitingMsgs)),
    NewWaitingMsgs = dict:erase(FirstId, WaitingMsgs),
    dict:store(MsgId, TRef, NewWaitingMsgs).

%% @private
-spec(remove_waiting(integer(), integer(), dict()) -> dict()).
remove_waiting(Src, Seq, WaitingMsgs) ->
    dict:filter(fun(<<Src2:?SRCADDR, Seq2:?SEQNUM, _TTL:?TTL, _Payload/binary>>, _Value) when Src == Src2, Seq == Seq2 -> false;
                   (<<_Src2:?SRCADDR, _Seq2:?SEQNUM, _TTL:?TTL, _Payload/binary>>, _Value) -> true end, WaitingMsgs).

%% @private
-spec(get_waiting(integer(), integer(), dict()) -> binary()).
get_waiting(Src, Seq, WaitingMsgs) ->
    Msgs = dict:fetch_keys(WaitingMsgs),
    hd(lists:filter(fun(<<Src2:?SRCADDR, Seq2:?SEQNUM, _TTL:?TTL, _Payload/binary>>) when Src == Src2, Seq == Seq2 -> true;
                       (<<_Src2:?SRCADDR, _Seq2:?SEQNUM, _TTL:?TTL, _Payload/binary>>) -> false end, Msgs)).

%% @private
-spec(find_waiting(integer(), integer(), dict()) -> reference() | error).
find_waiting(Src, Seq, WaitingMsgs) ->
    dict:fold(fun(<<Src2:?SRCADDR, Seq2:?SEQNUM, _TTL:?TTL, _Payload/binary>>, Value, AccIn) when Src == Src2, Seq == Seq2, AccIn == error ->
                        Value;
                 (<<_Src2:?SRCADDR, _Seq2:?SEQNUM, _TTL:?TTL, _Payload/binary>>, _Value, AccIn) ->
                        AccIn end, error, WaitingMsgs).

%% @private
-spec(record_received({integer(), integer()}, [{integer(), integer()}]) -> [{integer(), integer()}]).
record_received(MsgId, ReceivedMsgs) when length(ReceivedMsgs) < ?MAX_RECEIVED_MSG ->
    [MsgId|ReceivedMsgs];
record_received(_MsgId, ReceivedMsgs) ->
    ReceivedMsgs.

%% @private
-spec(already_received({integer(), integer()}, [{integer(), integer()}]) -> boolean()).
already_received(MsgId, ReceivedMsgs) ->
    lists:member(MsgId, ReceivedMsgs).

