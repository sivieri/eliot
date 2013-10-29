-module(i2cdev).
-export([init/0, setup/1, close/0, read/0, write/1, write_reg8/2, write_reg16/2, read_reg8/1, read_reg16/1]).
-on_load(init/0).

-define(APPNAME, i2cdev).

init() ->
    case code:priv_dir(?APPNAME) of
        {error, _} -> 
            error_logger:format("~w priv dir not found~n", [?APPNAME]),
            exit(error);
        PrivDir ->
            erlang:load_nif(filename:join([PrivDir, "i2cdev_drv"]), 0)
    end.

setup(_Device) ->
    erlang:nif_error(nif_not_loaded).

close() ->
    erlang:nif_error(nif_not_loaded).

read() ->
    erlang:nif_error(nif_not_loaded).

write(_Data) ->
    erlang:nif_error(nif_not_loaded).

write_reg8(_Register, _Data) ->
    erlang:nif_error(nif_not_loaded).

write_reg16(_Register, _Data) ->
    erlang:nif_error(nif_not_loaded).

read_reg8(_Register) ->
    erlang:nif_error(nif_not_loaded).

read_reg16(_Register) ->
    erlang:nif_error(nif_not_loaded).
