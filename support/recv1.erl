-module(recv1).
-export([recv1/0, start_i2c/0, loop2/0]).
-define(DEVICE, 16#54).
-define(ORDER, [10, 5, 6, 9, 8, 7, 11, 12, 14, 16, 17, 18, 13, 15, 4, 3, 2, 1]).
-define(LIGHT, 5).

recv1() ->
    spawn(fun() -> recv1:start_i2c() end),
    receive
        {Pid, {"param", "name"}} ->
            Pid ! {self(), ["OpenHouse receiver"]},
            recv1:recv1();
        {Pid, {"param", "operation"}} ->
            Pid ! {self(), ["GET"]},
            recv1:recv1();
        {Pid, {"param", "parameters"}} ->
            Pid ! {self(), []},
            recv1:recv1();
        {Pid, Other} ->
            Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
            recv1:recv1()
        end.

start_i2c() ->
    case find(openhouse, erlang:registered()) of
        true ->
            ok;
        false ->
            erlang:register(openhouse, self()),
            erlang:export(openhouse),
            i2cdev:setup(16#54),
            i2cdev:write_reg8(0, 1),
            i2cdev:write_reg8(16#13, 16#3F),
            i2cdev:write_reg8(16#14, 16#3F),
            i2cdev:write_reg8(16#15, 16#3F),
            i2cdev:write_reg8(16#16, 0),
            recv1:loop2()
    end.

loop2() ->
    receive
        {_RSSI, _SourceAddress, {mcp3002, Value}} ->
            MapValue = erlang:trunc(map(Value, 0, 1023, 1, 18)),
            io:format("DEBUG: Value ~p, MapValue ~p~n", [Value, MapValue]),
            lists:foreach(fun(I) when I =< MapValue -> i2cdev:write_reg8(lists:nth(I, ?ORDER), ?LIGHT band 16#FF),
                                                       i2cdev:write_reg8(16#16, 0);
                             (I) -> i2cdev:write_reg8(lists:nth(I, ?ORDER), 0 band 16#FF),
                                    i2cdev:write_reg8(16#16, 0) end, lists:seq(1, 18));
        Any ->
            io:format(standard_error, "Unknown message: ~p~n", [Any])
    end,
    recv1:loop2().

map(Value, InMin, InMax, OutMin, OutMax) ->
    (Value - InMin) * (OutMax - OutMin) / (InMax - InMin) + OutMin.

find(_, []) ->
    false;
find(Elem, [H|_]) when Elem == H ->
    true;
find(Elem, [_|T]) ->
    find(Elem, T).
