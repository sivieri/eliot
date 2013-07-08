-module(demo_recv).
-export([start/0]).
-define(CHANNEL, 0).
-define(SPEED, 500000).
-define(VALUES, [1, 3, 7, 15, 31, 63, 127, 255]).

% Public API

start() ->
    erlang:register(demo, self()),
    erlang:export(demo),
    spidev:setup(?CHANNEL, ?SPEED),
    loop(?VALUES).

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

map(Value, InMin, InMax, OutMin, OutMax) ->
    (Value - InMin) * (OutMax - OutMin) / (InMax - InMin) + OutMin.

