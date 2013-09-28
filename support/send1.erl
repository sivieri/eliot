-module(send1).
-export([send1/0, start/0, loop/0]).
-define(CHANNEL, 0).
-define(SPEED, 1000000).
-define(REMOTE_PROCESS, openhouse).
-define(MSG, [104, 0]).
-define(SLEEP, 1000).

send1() ->
    spawn(fun() -> send1:start() end),
    receive
        {Pid, {"param", "name"}} ->
            Pid ! {self(), ["OpenHouse sender"]},
            send1:send1();
        {Pid, {"param", "operation"}} ->
            Pid ! {self(), ["GET"]},
            send1:send1();
        {Pid, {"param", "parameters"}} ->
            Pid ! {self(), []},
            send1:send1();
        {Pid, Other} ->
            Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
            send1:send1()
        end.

start() ->
    spidev:setup(?CHANNEL, ?SPEED),
    send1:loop().

loop() ->
    {ok, [A, B]} = spidev:xfer(?CHANNEL, ?MSG),
    Res = ((A band 3) bsl 8) bor B,
    io:format("DEBUG: A ~p, B ~p, Res ~p~n", [A, B, Res]),
    {?REMOTE_PROCESS, all} ~ {mcp3002, Res},
    timer:sleep(?SLEEP),
    send1:loop().
