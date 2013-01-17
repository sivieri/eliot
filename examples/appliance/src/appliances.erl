-module(appliances).
-export([start/1, ac/1, dw/1, wm/1]).
-include("scenario.hrl").

% Public API

start([Type, Id]) ->
    TypeInt = erlang:list_to_integer(erlang:atom_to_list(Type)),
    if
        TypeInt == 2 ->
            dw(Id);
        TypeInt == 3 ->
            wm(Id);
        true ->
            ac(Id)
    end.

ac(Id) ->
    application:set_env(appliance, type, ac),
    application:set_env(appliance, params, [#parameter{name = temperature, type = temperature, value = 25, fixed = true}, #parameter{name = starttime, type = time}, #parameter{name = endtime, type = time, value = 6}]),
    appliance:start(Id).

dw(Id) ->
    application:set_env(appliance, type, dw),
    application:set_env(appliance, params, [#parameter{name = starttime, type = time}, #parameter{name = endtime, type = time, value = 1}]),
    appliance:start(Id).

wm(Id) ->
    application:set_env(appliance, type, wm),
    application:set_env(appliance, params, [#parameter{name = starttime, type = time}, #parameter{name = endtime, type = time, value = 1}]),
    appliance:start(Id).

% Private API
