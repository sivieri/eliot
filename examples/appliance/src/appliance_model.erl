-module(appliance_model).
-export([start_link/0, model/0]).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, model, []),
    register(model, Pid),
    erlang:export(Pid),
    {ok, Pid}.

model() ->
    receive
        {_RSSI, {_Source, Content}} ->
            case binary_to_term(Content) of
                Any ->
                    io:format("Appliance model: Unknown message ~p~n", [Any])
            end,
            model();
        Any ->
            io:format("Appliance model: Unknown message ~p~n", [Any]),
            model()
    end.

% Private API
