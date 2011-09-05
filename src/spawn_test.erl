-module(spawn_test).
-export([start/1, start/2, launch/1, test/0]).

start(Filename) ->
    Net = wsn:read_net(Filename),
    wsn:spawn_net(Net, ?MODULE, test).

start(Filename, Hosts) ->
    Net = wsn:read_net(Filename),
    wsn:spawn_net(Net, Hosts, ?MODULE, test).

launch(DestId) ->
    wsn:send_ignore_gain(get(myid), DestId, go),
    ok.

test() ->
    receive
        go ->
            wsn:spawn(get(myid), all, fun() -> io:format("~p~n", [get(myid)]) end),
            test();
        Any ->
            io:format("~p: waiting for messages...", [get(myid)]),
            Any,
            test()
    end.
