-module(appliance_task).
-export([start_link/0, appliance/0]).
-include("eliot.hrl").
-record(state, {sm = none}).

% Public API

start_link() ->
    appliance_handler:add_handler(),
    Pid = spawn_link(?MODULE, appliance, []),
    register(appliance, Pid),
    erlang:export(appliance),
    {ok, Pid}.

appliance() ->
    appliance(#state{}).

% Private API

appliance(#state{sm = SM} = State) ->
    receive
        {sm, NewSM} ->
            if
                SM == none ->
                    io:format("Appliance: Registering to SM ~p~n", [NewSM]),
                    utils:join_name(?NODENAME, NewSM) ! eliot_api:msg(term_to_binary(appliance)),
                    appliance(#state{sm = NewSM});
                SM == NewSM ->
                    appliance(State);
                true ->
                    io:format("Appliance: Already registered to SM ~p, changing to ~p~n", [SM, NewSM]),
                    utils:join_name(?NODENAME, NewSM) ! eliot_api:msg(term_to_binary(appliance)),
                    appliance(#state{sm = NewSM})
            end;
        Any ->
            io:format("Appliance: Unknown message ~p~n", [Any]),
            appliance(State)
    end.
