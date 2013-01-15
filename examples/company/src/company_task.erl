-module(company_task).
-export([start_link/0, company/0, set_cap/1]).
-include("eliot.hrl").
-record(state, {sm = none}).

% Public API

start_link() ->
    company_handler:add_handler(),
    Pid = spawn_link(?MODULE, company, []),
    register(company, Pid),
    erlang:export(company),
    {ok, Pid}.

company() ->
    company(#state{}).

set_cap(Cap) ->
    company ! {cap, Cap}.

% Private API

company(#state{sm = SM} = State) ->
    receive
        {cap, Cap} ->
            Dest = utils:join_name(?NODENAME, SM),
            {sm, Dest} ~ eliot_api:msg(term_to_binary({company, cap, Cap})),
            company(State);
        {sm, {_NodeName, NodeIP}} ->
            if
                SM == none ->
                    io:format("Company: Registering to SM ~p~n", [NodeIP]),
                    {sm, utils:join_name(?NODENAME, NodeIP)} ! eliot_api:msg(term_to_binary('company')),
                    company(#state{sm = NodeIP});
                SM == NodeIP ->
                    company(State);
                true ->
                    io:format("Company: Already registered to SM ~p, changing to ~p~n", [SM, NodeIP]),
                    utils:join_name(?NODENAME, NodeIP) ! eliot_api:msg(term_to_binary(company)),
                    company(#state{sm = NodeIP})
            end;
        {_RSSI, {Source, Content}} ->
            case binary_to_term(Content) of
                {sm, current, Value} ->
                    io:format("Company: ~p is consuming ~p KWh~n", [Source, Value]);
                Any ->
                    io:format("Company: Unknown binary message ~p~n", [Any])
            end,
            company(State);
        Any ->
            io:format("Company: Unknown message ~p~n", [Any]),
            company(State)
    end.
