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
        {sm, NewSM} ->
            if
                SM == none ->
                    io:format("Company: Registering to SM ~p~n", [NewSM]),
                    utils:join_name(?NODENAME, NewSM) ! eliot_api:msg(term_to_binary(company)),
                    company(#state{sm = NewSM});
                SM == NewSM ->
                    company(State);
                true ->
                    io:format("Company: Already registered to SM ~p, changing to ~p~n", [SM, NewSM]),
                    utils:join_name(?NODENAME, NewSM) ! eliot_api:msg(term_to_binary(company)),
                    company(#state{sm = NewSM})
            end;
        Any ->
            io:format("Company: Unknown message ~p~n", [Any]),
            company(State)
    end.
