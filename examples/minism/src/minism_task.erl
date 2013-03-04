-module(minism_task).
-export([start_link/0, minism/0, reset/0]).
-include("scenario.hrl").
-include("eliot.hrl").
-define(TIMER, 10 * 1000).
-define(NAME, 'minism_algorithm').
-record(state, {sm = none}).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, minism, []),
    register(sm, Pid),
    erlang:export(sm),
    {ok, Pid}.

minism() ->
    minism(#state{}).

reset() ->
    sm ! reset.

% Private API

minism(#state{sm = SM} = State) ->
    receive
        {_RSSI, Source, Content} ->
            case Content of
                <<?SM:8/unsigned-little-integer>> ->
                    if
                        SM == none ->
                            io:format("Appliance: Registering to SM ~p~n", [Source]),
                            Dest = {sm, eliot_api:ip_to_node(Source)},
                            Name = data:encode_name(?NAME),
                            {_, ModuleBinary, _} = code:get_object_code(?NAME),
                            Hash = crypto:sha(ModuleBinary),
                            Dest ~ <<?APPLIANCE_LOCAL:8/unsigned-little-integer, Hash:20/binary, Name/binary, ModuleBinary/binary>>,
                            minism(State#state{sm = Source});
                        SM == Source ->
                            minism(State);
                        true ->
                            io:format("Appliance: Already registered to SM ~p, changing to ~p~n", [SM, Source]),
                            Dest = {sm, eliot_api:ip_to_node(Source)},
                            Name = data:encode_name(?NAME),
                            {_, ModuleBinary, _} = code:get_object_code(?NAME),
                            Hash = crypto:sha(ModuleBinary),
                            Dest ~ <<?APPLIANCE_LOCAL:8/unsigned-little-integer, Hash:20/binary, Name/binary, ModuleBinary/binary>>,
                            minism(State#state{sm = Source})
                    end;
                <<?RESET:8/unsigned-little-integer>> ->
                    minism(State#state{sm = none}); 
                Any ->
                    io:format("Appliance: Unknown binary message ~p~n", [Any])
            end,
            minism(State);
        Any ->
            io:format("Appliance: Unknown message ~p~n", [Any]),
            minism(State)
    end.
