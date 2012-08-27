-module(appliances).
-export([ac/1, dw/1, wm/1]).
-include("scenario.hrl").

% Public API

ac(Id) ->
    application:set_env(appliance, type, ac),
    application:set_env(appliance, params, [#parameter{name = temperature, type = temperature}, #parameter{name = starttime, type = time}, #parameter{name = endtime, type = time}]),
    appliance:start(Id).

dw(Id) ->
    application:set_env(appliance, type, dw),
    application:set_env(appliance, params, [#parameter{name = starttime, type = time}, #parameter{name = endtime, type = time}]),
    appliance:start(Id).

wm(Id) ->
    application:set_env(appliance, type, wm),
    application:set_env(appliance, params, [#parameter{name = starttime, type = time}, #parameter{name = endtime, type = time}]),
    appliance:start(Id).

% Private API
