%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Main application.
-module(wsn_app).
-behaviour(application).
-export([start/2, stop/1]).

% Public API

start(_StartType, _StartArgs) ->
    wsn_sup:start_link().

stop(_State) ->
    ok.

% Private API
