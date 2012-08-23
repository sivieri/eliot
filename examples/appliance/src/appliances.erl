-module(appliances).
-export([]).

% Public API

ac(Id) ->
    put(params, [#parameter{name = temperature, type = temperature}, #parameter{name = starttime, type = time}, #parameter{name = endtime, type = time}]),
    appliance:start(Id).

dishwasher() ->
    put(params, [#parameter{name = starttime, type = time}, #parameter{name = endtime, type = time}]),
    appliance:start(Id).

% Private API
