-module(i2cglow).
-export([test/0]).

test() ->
    ok = i2cdev:setup(16#54),
    ok = i2cdev:write_reg8(0, 1),
    ok = i2cdev:write_reg8(16#13, 16#3F),
    ok = i2cdev:write_reg8(16#14, 16#3F),
    ok = i2cdev:write_reg8(16#15, 16#3F),
    ok = i2cdev:write_reg8(16#16, 0),
    lists:foreach(fun(I) -> i2cdev:write_reg8(I, 10 band 16#FF),
                            i2cdev:write_reg8(16#16, 0),
                            timer:sleep(1000),
                            i2cdev:write_reg8(I, 0 band 16#FF),
                            i2cdev:write_reg8(16#16, 0) end, lists:seq(1, 18)),
    i2cdev:close().

