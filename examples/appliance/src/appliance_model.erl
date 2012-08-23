-module(appliance_model).
-export([start_link/0, model/0]).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, model, []),
    register(model, Pid),
    {ok, Pid}.

model() ->
    receive
        Any ->
            io:format("Appliance model: Unknown message ~p~n", [Any]),
            model()
    end.

% Private API
