-module(sm_current).
-export([start_link/1, current/1]).
-include("eliot.hrl").
-define(TIMER, 60 * 1000).
-record(state, {company = none}).

% Public API

start_link(Company) ->
    Pid = spawn_link(?MODULE, current, [#state{company = Company}]),
    erlang:send_after(?TIMER, Pid, send),
    {ok, Pid}.

% Private API

current(#state{company = Company} = State) ->
    receive
        send ->
            Dest = utils:join_name(?NODENAME, Company),
            {company, Dest} ! eliot_api:msg(term_to_binary({sm, current, 100})),
            erlang:send_after(?TIMER, self(), send);
        Any ->
            io:format("Sm Current: Unknown message ~p~n", [Any])
    end,
    current(State).
