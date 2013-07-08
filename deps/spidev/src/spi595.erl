-module(spi595).
-export([test/0]).

test() ->
    Values = [1, 3, 7, 15, 31, 63, 127, 255],
    Reset = [0],
    spidev:setup(0, 500000),
    lists:foreach(fun(V) -> spidev:xfer(0, [V]),
                            timer:sleep(1000) end, Values),
    spidev:xfer(0, Reset),
    spidev:close(0).
