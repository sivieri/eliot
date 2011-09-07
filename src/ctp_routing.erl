%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Routing engine, as suggested by the TinyOS implementation
%% protocol.
%% @reference <a href="http://www.tinyos.net/tinyos-2.x/doc/html/tep123.html">TinyOS TEP 123</a>
-module(ctp_routing).
-include("ctp.hrl").
-export([routing_engine/1]).

% Public API

%% @doc Start the engine.
%% @spec routing_engine(atom()) -> none()
-spec(routing_engine(atom()) -> none()).
routing_engine(NodeId) ->
    routing_engine(NodeId, dict:new()).

% Private API

%% @private
-spec(routing_engine(atom(), dict()) -> none()).
routing_engine(NodeId, Neighbors) ->
    receive
        {Pid, lower} ->
            Pid ! {self(), lower_node(Neighbors)},
            routing_engine(NodeId, Neighbors);
        {_Pid, update_routing, SourceId, Msg} ->
            routing_engine(NodeId, update_dst(Neighbors, SourceId, Msg));
        {_Pid, update_ack, SourceId} ->
            routing_engine(NodeId, update_dst_ack(Neighbors, SourceId));
        Any ->
            io:format("Routing engine: received ~p~n", [Any]),
            routing_engine(NodeId, Neighbors)
    end.

%% @doc For a neighbor node to be considered the best parent, it must have an ETX greater
%% than 1000, which is the default value of a neighbor from which we have received only
%% one beacon.
%% If there are only neighbors with 1000 as ETX, then choose one randomly, and let's
%% hope it will receive the message.
%% @private
-spec(lower_node(dict()) -> {atom(), integer()}).
lower_node(Dict) ->
    dict:fold(fun(Key, Value, {Node, N}) ->
                      if
                          Value#neighbor.distance < N ->
                              {Key, Value#neighbor.distance};
                          true ->
                              {Node, N}
                      end end, {none, 1000}, Dict).

%% @doc The link distance is calculated as the difference between the current sequence number of
%% the routing beacon and the last registered one from the same node.
%% @private
-spec(update_dst(dict(), atom(), #routing{}) -> dict()).
update_dst(Neighbors, SourceId, RoutingMsg) ->
    case dict:find(SourceId, Neighbors) of
        {ok, Neighbor} ->
            Last = Neighbor#neighbor.last,
            NewLast = RoutingMsg#routing.seqno,
            NewNeighbor = Neighbor#neighbor{distance = RoutingMsg#routing.etx + NewLast - Last, last = NewLast};
        error ->
            NewNeighbor = #neighbor{node = SourceId, last = RoutingMsg#routing.seqno}
    end,
    dict:store(SourceId, NewNeighbor, Neighbors).

%% @private
-spec(update_dst_ack(dict(), atom()) -> dict()).
update_dst_ack(Neighbors, SourceId) ->
    Neighbor = dict:fetch(SourceId, Neighbors),
    OldDist = Neighbor#neighbor.distance,
    NewNeighbor = Neighbor#neighbor{distance = OldDist + 1},
    dict:store(SourceId, NewNeighbor, Neighbors).
