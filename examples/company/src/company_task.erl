-module(company_task).
-export([start_link/0, company/0, set_cap/1]).
-include("eliot.hrl").
-include("scenario.hrl").
-record(state, {sm = none, cap = 3000}).

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

company(#state{sm = SM, cap = Cap} = State) ->
    receive
        {cap, NewCap} ->
            Dest = utils:join_name(?NODENAME, SM),
            {sm, Dest} ~ eliot_api:msg(<<?COMPANY:8/unsigned-little-integer, Cap:16/unsigned-little-integer>>),
            company(State#state{cap = NewCap});
        {_RSSI, {{_NodeId, NodeIP} = Source, Content}} ->
            case Content of
                <<?CONSUMPTION:8/unsigned-little-integer, Value:16/unsigned-little-integer>> ->
                    io:format("Company: ~p is consuming ~p KWh~n", [Source, Value]);
                <<?SM:8/unsigned-little-integer>> ->
                    if
                        SM == none ->
                            io:format("Company: Registering to SM ~p~n", [NodeIP]),
                            {sm, utils:join_name(?NODENAME, NodeIP)} ~ eliot_api:msg(<<?COMPANY:8/unsigned-little-integer, Cap:16/unsigned-little-integer>>),
                            company(#state{sm = NodeIP});
                        SM == NodeIP ->
                            company(State);
                        true ->
                            io:format("Company: Already registered to SM ~p, changing to ~p~n", [SM, NodeIP]),
                            utils:join_name(?NODENAME, NodeIP) ~ eliot_api:msg(<<?COMPANY:8/unsigned-little-integer, Cap:16/unsigned-little-integer>>),
                            company(#state{sm = NodeIP})
                    end;
                Any ->
                    io:format("Company: Unknown binary message ~p~n", [Any])
            end,
            company(State);
        Any ->
            io:format("Company: Unknown message ~p~n", [Any]),
            company(State)
    end.
