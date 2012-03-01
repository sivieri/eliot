%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc RSSI producer.
-module(eliot_rssi).
-export([init/0, rssi/1]).
-on_load(init/0).

% Public API

init() ->
    case code:priv_dir(eliot) of
        {error, _} ->
            io:format("~w priv directory not found!", [eliot]),
            exit(error);
        PrivDir ->
            erlang:load_nif(filename:join([PrivDir, "eliot_rssi"]), 0)
    end.

rssi(InterfaceName) ->
    erlang:nif_error(nif_not_loaded).

% Private API
