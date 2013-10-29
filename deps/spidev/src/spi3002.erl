-module(spi3002).
-export([test/0]).

test() ->
    spidev:setup(0, 1000000),
    case spidev:xfer(0, [104,0]) of
        {ok, [A, B]} ->
            Res = ((A band 3) bsl 8) bor B,
            io:format("Partials: ~p ~p~nResult: ~p~n", [A, B, Res]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    spidev:close(0).
