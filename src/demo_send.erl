-module(demo_send).
-export([start/1]).
-define(CHANNEL, 0).
-define(SPEED, 1000000).
-define(REMOTE_PROCESS, demo).
-define(MSG, [104, 0]).
-define(SLEEP, 1000).

% Public API

start(RemoteNode) ->
    spidev:setup(?CHANNEL, ?SPEED),
    loop(RemoteNode).

% Private API

loop(RemoteNode) ->
    {ok, [A, B]} = spidev:xfer(?CHANNEL, ?MSG),
    Res = ((A band 3) bsl 8) bor B,
    io:format("DEBUG: A ~p, B ~p, Res ~p~n", [A, B, Res]),
    {?REMOTE_PROCESS, RemoteNode} ~ {mcp3002, Res},
    timer:sleep(?SLEEP),
    loop(RemoteNode).

