%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Main application.
-module(eliot_app).
-behaviour(application).
-export([start/2, stop/1]).

% Public API

start(_StartType, _StartArgs) ->
    case application:get_env(eliot, simulation) of
        {ok, true} ->
            ok;
        _Any ->
            application:set_env(eliot, simulation, false)
    end,
    eliot_sup:start_link().

stop(_State) ->
    ok.

% Private API
