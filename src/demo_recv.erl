-module(demo_recv).
-export([start_spi/0, start_i2c/0]).
-define(CHANNEL, 0).
-define(DEVICE, 16#54).
-define(SPEED, 500000).
-define(VALUES, [1, 3, 7, 15, 31, 63, 127, 255]).
-define(ORDER, [10, 5, 6, 9, 8, 7, 11, 12, 14, 16, 17, 18, 13, 15, 4, 3, 2, 1]).
-define(LIGHT, 5).

% Public API

start_spi() ->
    erlang:register(demo, self()),
    erlang:export(demo),
    spidev:setup(?CHANNEL, ?SPEED),
    loop(?VALUES).

start_i2c() ->
    erlang:register(demo, self()),
    erlang:export(demo),
    i2cdev:setup(16#54),
    i2cdev:write_reg8(0, 1),
    i2cdev:write_reg8(16#13, 16#3F),
    i2cdev:write_reg8(16#14, 16#3F),
    i2cdev:write_reg8(16#15, 16#3F),
    i2cdev:write_reg8(16#16, 0),
    loop2().

% Private API

loop(States) ->
    receive
        {_RSSI, _SourceAddress, {mcp3002, Value}} ->
            MapValue = erlang:trunc(map(Value, 0, 1023, 1, 8)),
            Leds = lists:nth(MapValue, States),
            io:format("DEBUG: Value ~p, MapValue ~p, Leds ~p~n", [Value, MapValue, Leds]),
            spidev:xfer(?CHANNEL, [Leds]);
        Any ->
            io:format(standard_error, "Unknown message: ~p~n", [Any])
    end,
    loop(States).

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
    loop2().

map(Value, InMin, InMax, OutMin, OutMax) ->
    (Value - InMin) * (OutMax - OutMin) / (InMax - InMin) + OutMin.

