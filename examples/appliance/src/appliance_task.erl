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
        {sm, {_NodeName, NodeIP}} ->
            if
                SM == none ->
                    io:format("Appliance: Registering to SM ~p~n", [NodeIP]),
                    Dest = {sm, utils:join_name(?NODENAME, NodeIP)},
                    Dest ! eliot_api:msg(term_to_binary('appliance')),
                    {ok, Params} = application:get_env(appliance, params),
                    Dest ! eliot_api:msg(term_to_binary({appliance, data, whereis(model), Params})),
                    appliance(#state{sm = NodeIP});
                SM == NodeIP ->
                    appliance(State);
                true ->
                    io:format("Appliance: Already registered to SM ~p, changing to ~p~n", [SM, NodeIP]),
                    utils:join_name(?NODENAME, NodeIP) ! eliot_api:msg(term_to_binary(appliance)),
                    appliance(#state{sm = NodeIP})
            end;
        {schedule, Params} ->
            io:format("Appliance: New schedule ~p has been decided by the SM~n", [Params]),
            appliance(State);
        {eval, CurrentTime, Params} ->
            io:format("Appliance: Evaluation of parameters ~p at time ~p~n", [Params, CurrentTime]),
            Ans = eliot_api:lpc(model, {eval, CurrentTime, Params}),
            Dest = utils:join_name(?NODENAME, SM),
            {algorithm, Dest} ! eliot_api:msg(Ans),
            appliance(State);
        Any ->
            io:format("Appliance: Unknown message ~p~n", [Any]),
            appliance(State)
    end.
