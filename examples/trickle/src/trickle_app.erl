%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @reference <a href="http://portal.acm.org/citation.cfm?id=1251177">Trickle</a>
-module(trickle_app).
-behaviour(application).
-export([start/2, stop/1]).

% Public API

start(_StartType, _StartArgs) ->
    trickle_sup:start_link().

stop(_State) ->
    ok.

% Private API
