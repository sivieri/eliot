-module(appliance_task).
-export([start_link/0, appliance/0]).
-include("eliot.hrl").
-include("scenario.hrl").
-record(state, {sm = none}).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, appliance, []),
    register(appliance, Pid),
    erlang:export(appliance),
    erlang:export(Pid),
    {ok, Pid}.

appliance() ->
    appliance(#state{}).

% Private API

appliance(#state{sm = SM} = State) ->
    receive
        {_RSSI, {{_NodeId, NodeIP} = _Source, Content}} ->
            case Content of
                <<?SM:8/unsigned-little-integer>> ->
                    if
                        SM == none ->
                            io:format("Appliance: Registering to SM ~p~n", [NodeIP]),
                            Dest = {sm, utils:join_name(?NODENAME, NodeIP)},
                            Dest ~ eliot_api:msg(<<?APPLIANCE:8/unsigned-little-integer>>),
                            {ok, Params} = application:get_env(appliance, params),
                            Bin1 = erlang:term_to_binary(self()),
                            Bin2 = data:encode_params(Params),
                            Dest ~ eliot_api:msg(<<?APPLIANCE:8/unsigned-little-integer, Bin1/binary, Bin2/binary>>),
                            appliance(#state{sm = NodeIP});
                        SM == NodeIP ->
                            appliance(State);
                        true ->
                            io:format("Appliance: Already registered to SM ~p, changing to ~p~n", [SM, NodeIP]),
                            utils:join_name(?NODENAME, NodeIP) ~ eliot_api:msg(term_to_binary(appliance)),
                            appliance(#state{sm = NodeIP})
                    end;
                <<?SCHEDULE:8/unsigned-little-integer, Other/binary>> ->
                    Params = data:decode_params(Other),
                    io:format("Appliance: New schedule ~p has been decided by the SM~n", [Params]);
                <<?EVAL:8/unsigned-little-integer, CurrentTime:8/unsigned-little-integer, Other/binary>> ->
                    Params = data:decode_params(Other),
                    io:format("Appliance: Evaluation of parameters ~p at time ~p~n", [Params, CurrentTime]),
                    Ans = eliot_api:lpc(model, {eval, CurrentTime, Params}),
                    Dest = utils:join_name(?NODENAME, SM),
                    {algorithm, Dest} ~ eliot_api:msg(<<?EVAL:8/unsigned-little-integer, Ans:16/unsigned-little-integer>>);
                Any ->
                    io:format("Appliance: Unknown binary message ~p~n", [Any])
            end,
            appliance(State);
        Any ->
            io:format("Appliance: Unknown message ~p~n", [Any]),
            appliance(State)
    end.
