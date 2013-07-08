-module(spidev).
-export([init/0, setup/2, xfer/2, close/1]).
-on_load(init/0).

-define(APPNAME, spidev).

init() ->
    case code:priv_dir(?APPNAME) of
        {error, _} -> 
            error_logger:format("~w priv dir not found~n", [?APPNAME]),
            exit(error);
        PrivDir ->
            erlang:load_nif(filename:join([PrivDir, "spidev_drv"]), 0)
    end.

setup(_Channel, _Speed) ->
    erlang:nif_error(nif_not_loaded).

xfer(_Channel, _Data) ->
    erlang:nif_error(nif_not_loaded).

close(_Port) ->
    erlang:nif_error(nif_not_loaded).
