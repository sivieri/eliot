%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Test module for remote spawn capabilities of WSN-Erlang framework.
-module(spawn_test).
-export([start/1, start/2, launch/1, test/0]).

%% @doc Start the simulation with the given topology.
%% @spec start(string()) -> [{ok, reference()}|{error, string()}]
-spec(start(string()) -> [{ok, reference()}|{error, string()}]).
start(Filename) ->
    Net = wsn:read_net(Filename),
    wsn:spawn_net(Net, ?MODULE, test).

%% @doc Start the simulation with the given topology on the given hosts.
%% @spec start(string(), [{atom(), integer()}]) -> [{ok, reference()}|{error, string()}]
-spec(start(string(), [{atom(), integer()}]) -> [{ok, reference()}|{error, string()}]).
start(Filename, Hosts) ->
    Net = wsn:read_net(Filename),
    wsn:spawn_net(Net, Hosts, ?MODULE, test).

%% @doc Start the spawning process from the given node.
%% @spec launch(atom()) -> ok
-spec(launch(atom()) -> ok).
launch(DestId) ->
    wsn:send_ignore_gain(get(myid), DestId, go),
    ok.

%% @private
-spec(test() -> none()).
test() ->
    receive
        go ->
            wsn:spawn(get(myid), all, fun() -> io:format("~p~n", [node()]) end),
            test();
        Any ->
            io:format("~p: received ~p~n", [get(myid), Any]),
            io:format("~p: waiting for messages...~n", [get(myid)]),
            test()
    end.
