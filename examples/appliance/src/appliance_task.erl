-module(appliance_task).
-export([start_link/0, appliance/0]).
-include("eliot.hrl").
-include("scenario.hrl").
-record(state, {sm = none}).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, appliance, []),
    register(sm, Pid),
    erlang:export(sm),
    {ok, Pid}.

appliance() ->
    appliance(#state{}).

% Private API

appliance(#state{sm = SM} = State) ->
    receive
        {_RSSI, Source, Content} ->
            case Content of
                <<?SM:8/unsigned-little-integer>> ->
                    if
                        SM == none ->
                            io:format("Appliance: Registering to SM ~p~n", [Source]),
                            Dest = {sm, eliot_api:ip_to_node(Source)},
                            Dest ~ <<?APPLIANCE:8/unsigned-little-integer>>,
                            {ok, Params} = application:get_env(appliance, params),
                            Bin = data:encode_params(Params),
                            Dest ~ <<?APPLIANCE:8/unsigned-little-integer, Bin/binary>>,
                            appliance(#state{sm = Source});
                        SM == Source ->
                            appliance(State);
                        true ->
                            io:format("Appliance: Already registered to SM ~p, changing to ~p~n", [SM, Source]),
                            Dest = {sm, eliot_api:ip_to_node(Source)},
                            Dest ~ <<?APPLIANCE:8/unsigned-little-integer>>,
                            appliance(#state{sm = Source})
                    end;
                <<?SCHEDULE:8/unsigned-little-integer, Other/binary>> ->
                    Params = data:decode_params(Other),
                    io:format("Appliance: New schedule ~p has been decided by the SM~n", [Params]);
                <<?EVAL:8/unsigned-little-integer, CurrentTime:8/unsigned-little-integer, Other/binary>> ->
                    Params = data:decode_params(Other),
                    io:format("Appliance: Evaluation of parameters ~p at time ~p~n", [Params, CurrentTime]),
                    Ans = eliot_api:lpc(model, {eval, CurrentTime, Params}),
                    Dest = {alg, eliot_api:ip_to_node(Source)},
                    Dest ~ <<?EVAL:8/unsigned-little-integer, Ans:16/unsigned-little-integer>>;
                Any ->
                    io:format("Appliance: Unknown binary message ~p~n", [Any])
            end,
            appliance(State);
        Any ->
            io:format("Appliance: Unknown message ~p~n", [Any]),
            appliance(State)
    end.
