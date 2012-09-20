-module(miniapp_task).
-export([start_link/0, miniapp/0]).
-include("eliot.hrl").
-record(state, {sm = none}).

% Public API

start_link() ->
    miniapp_handler:add_handler(),
    Pid = spawn_link(?MODULE, miniapp, []),
    register(miniapp, Pid),
    erlang:export(miniapp),
    erlang:export(Pid),
    {ok, Pid}.

miniapp() ->
    miniapp(#state{}).

% Private API

miniapp(#state{sm = SM} = State) ->
    receive
        {sm, {_NodeName, NodeIP}} ->
            if
                SM == none ->
                    io:format("Appliance: Registering to SM ~p~n", [NodeIP]),
                    Dest = {sm, utils:join_name(?NODENAME, NodeIP)},
                    Dest ! eliot_api:msg(term_to_binary('appliance')),
                    {Code, Hash} = code(),
                    Dest ! eliot_api:msg(term_to_binary({appliance, code, 'miniapp_model', Code, Hash, 'miniapp_model.erl'})),
                    miniapp(#state{sm = NodeIP});
                SM == NodeIP ->
                    miniapp(State);
                true ->
                    io:format("Appliance: Already registered to SM ~p, changing to ~p~n", [SM, NodeIP]),
                    utils:join_name(?NODENAME, NodeIP) ! eliot_api:msg(term_to_binary(appliance)),
                    miniapp(#state{sm = NodeIP})
            end;
        {_RSSI, {_Source, Content}} ->
            case binary_to_term(Content) of
                {schedule, Params} ->
                    io:format("Appliance: New schedule ~p has been decided by the SM~n", [Params]);
                Any ->
                    io:format("Appliance: Unknown binary message ~p~n", [Any])
            end,
            miniapp(State);
        Any ->
            io:format("Appliance: Unknown message ~p~n", [Any]),
            miniapp(State)
    end.

code() ->
    Code = code:get_object_code('miniapp_model'),
    Hash = utils:code_hash(Code),
    {Code, Hash}.
