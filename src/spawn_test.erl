%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Test module for remote spawn capabilities of WSN-Erlang framework.
-module(spawn_test).
-export([start/1, start_simulation/1, start_simulation/2, launch/1, test/0]).

%% @doc Start a real network.
%% @spec start([{atom(), integer()}]) -> ok
-spec(start([{atom(), integer()}]) -> ok).
start(Hosts) ->
    wsn:spawn_net(Hosts, ?MODULE, test).

%% @doc Start the simulation with the given topology.
%% @spec start_simulation(string()) -> [{ok, reference()}|{error, string()}]
-spec(start_simulation(string()) -> [{ok, reference()}|{error, string()}]).
start_simulation(Filename) ->
    Net = wsn:read_net(Filename),
    wsn:spawn_net(Net, [], ?MODULE, test).

%% @doc Start the simulation with the given topology on the given hosts.
%% @spec start_simulation(string(), [{atom(), integer()}]) -> [{ok, reference()}|{error, string()}]
-spec(start_simulation(string(), [{atom(), integer()}]) -> [{ok, reference()}|{error, string()}]).
start_simulation(Filename, Hosts) ->
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
        {_SourceId, _RSSI, go} ->
            wsn:spawn(get(myid), all, fun() -> io:format("~p~n", [node()]) end),
            test();
        Any ->
            io:format("~p: received ~p~n", [get(myid), Any]),
            io:format("~p: waiting for messages...~n", [get(myid)]),
            test()
    end.
