-module(company_task).
-export([start_link/0, company/0]).
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

% Private API

company(#state{sm = SM} = State) ->
    receive
        {sm, {_NodeName, NodeIP} ->
            if
                SM == none ->
                    io:format("Company: Registering to SM ~p~n", [NodeIP]),
                    utils:join_name(?NODENAME, NodeIP) ! eliot_api:msg(term_to_binary(company)),
                    company(#state{sm = NodeIP});
                SM == NodeIP ->
                    company(State);
                true ->
                    io:format("Company: Already registered to SM ~p, changing to ~p~n", [SM, NodeIP]),
                    utils:join_name(?NODENAME, NodeIP) ! eliot_api:msg(term_to_binary(company)),
                    company(#state{sm = NodeIP})
            end;
        Any ->
            io:format("Company: Unknown message ~p~n", [Any]),
            company(State)
    end.
