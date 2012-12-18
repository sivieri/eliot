-module(eliot_spidev).
-export([init/0, open/1, close/0, xfer2/1]).
-on_load(init/0).

init() ->
    case code:priv_dir(eliot) of
        {error, _} -> 
            error_logger:format("~w priv dir not found~n", [eliot]),
            exit(error);
        PrivDir ->
            erlang:load_nif(filename:join([PrivDir, "spidev_drv"]), 0)
    end.

open(_Device) ->
    erlang:nif_error(nif_not_loaded).

close() ->
    erlang:nif_error(nif_not_loaded).

xfer2(_Data) ->
    erlang:nif_error(nif_not_loaded).

