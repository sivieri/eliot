-module(appliances).
-export([ac/1, dishwasher/1]).
-include("scenario.hrl").

% Public API

ac(Id) ->
    put(params, [#parameter{name = temperature, type = temperature}, #parameter{name = starttime, type = time}, #parameter{name = endtime, type = time}]),
    appliance:start(Id).

dishwasher(Id) ->
    put(params, [#parameter{name = starttime, type = time}, #parameter{name = endtime, type = time}]),
    appliance:start(Id).

% Private API
