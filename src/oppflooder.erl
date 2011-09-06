%% @author Gianpaolo Cugola <cugola@elet.polimi.it>
%% @doc Opportunistic flooder implementation.
-module(oppflooder).
-export([start/1, start/2, flood/0]).

% Public API

%% @doc Start the simulation with the given topology;
%% the root node (zero) will start the flood.
%% @spec start(string()) -> [{ok, reference()}|{error, string()}]
-spec(start(string()) -> [{ok, reference()}|{error, string()}]).
start(FileName) ->
    Net=wsn:read_net(FileName),
    wsn:spawn_net(Net, ?MODULE, flood),
    'mote_0' ! resend,
    timer:kill_after(5000,forwarder),
    lists:map(fun(X) -> timer:kill_after(5000,X) end, element(1,Net)).

%% @doc Start the simulation with the given topology on the given hosts;
%% the root node (zero) will start the flood.
%% @spec start(string(), [atom()]) -> [{ok, reference()}|{error, string()}]
-spec(start(string(), [atom()]) -> [{ok, reference()}|{error, string()}]).
start(FileName, Hosts) ->
    Net=wsn:read_net(FileName),
    wsn:spawn_net(Net, Hosts, ?MODULE, flood),
    wsn:send_ignore_gain(get(myid), 'mote_0', resend),
    timer:kill_after(5000,forwarder),
    lists:map(fun(X) -> timer:kill_after(5000,X) end, element(1,Net)).

%% @doc The flood implementation for the single node.
%% @spec flood() -> none()
-spec(flood() -> none()).
flood() ->
    flood([], dict:new(), 0).

% Private API

%% @private
-spec(flood([{integer(), integer()}], dict(), integer()) -> none()).
flood(ReceivedMsgs, WaitingMsgs, NextMsgNum) ->
    receive
	resend ->
	    MsgId = {get(myaddr), NextMsgNum},
	    wsn:send(get(myid), all, {MsgId,"Message from "++atom_to_list(get(myid))}),
	    flood(record_received(MsgId, ReceivedMsgs), WaitingMsgs, (NextMsgNum+1) rem 256);
	{MsgId, MsgData} ->
	    io:format("~p: Timer expired for message ~p sending it~n", [get(myid), MsgId]),
	    wsn:send(get(myid), all, {MsgId, MsgData}),
	    flood(ReceivedMsgs, remove_waiting(MsgId, WaitingMsgs), NextMsgNum);
	{SourceId, RSSI, {MsgId, MsgData}} ->
	    io:format("~p: Received ~p from ~p with RSSI=~p~n", [get(myid), MsgId, SourceId, RSSI]),
	    case find_waiting(MsgId, WaitingMsgs) of
		{ok, TRef} ->
		    io:format("~p: same msg in my waiting queue, cancelling~n", [get(myid)]),
		    erlang:cancel_timer(TRef),
		    flood(ReceivedMsgs, remove_waiting(MsgId, WaitingMsgs), NextMsgNum);
		error ->
		    case already_received(MsgId, ReceivedMsgs) of
			true -> 
			    io:format("~p: already received this msg~n", [get(myid)]),
			    flood(ReceivedMsgs, WaitingMsgs, NextMsgNum);
			false ->
			    Delay = 1000 + RSSI*10 + random:uniform(100),
			    io:format("~p: forwarding msg in ~p ms~n", [get(myid), Delay]),
			    TRef = erlang:send_after(Delay, self(), {MsgId, MsgData}),
			    flood(record_received(MsgId, ReceivedMsgs), add_waiting(MsgId, TRef, WaitingMsgs), NextMsgNum)
		    end
	    end
    end.

%% @private
-spec(add_waiting(integer(), reference(), dict()) -> dict()).
add_waiting(MsgId, TRef, WaitingMsgs) ->
    % TODO: add only if there is space and remove oldest, cancelling timer
    dict:store(MsgId, TRef, WaitingMsgs).

%% @private
-spec(remove_waiting(integer(), dict()) -> dict()).
remove_waiting(MsgId, WaitingMsgs) ->
    dict:erase(MsgId, WaitingMsgs).
    
%% Returns {ok, TRef} | error.
%% @private
-spec(find_waiting(integer(), dict()) -> {ok, reference()} | error).
find_waiting(MsgId, WaitingMsgs) ->
    dict:find(MsgId, WaitingMsgs).

%% @private
-spec(record_received({atom(), integer()}, [{integer(), integer()}]) -> [{integer(), integer()}]).
record_received(MsgId, ReceivedMsgs) ->
    % TODO: add only if there is space and remove oldest, cancelling timer
    [MsgId|ReceivedMsgs].

%% @private
-spec(already_received(integer(), [{integer(), integer()}]) -> boolean()).
already_received(MsgId, ReceivedMsgs) ->
    lists:member(MsgId, ReceivedMsgs).
