%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Main application.
-module(wsn_app).
-behaviour(application).
-export([start/2, stop/1]).

% Public API

start(_StartType, _StartArgs) ->
    case application:get_env(wsn, simulation) of
        {ok, true} ->
            ok;
        _Any ->
            application:set_env(wsn, simulation, false)
    end,
    wsn_sup:start_link().

stop(_State) ->
    ok.

% Private API
