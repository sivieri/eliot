-module(sm_current).
-export([start_link/1, current/1]).
-include("eliot.hrl").
-include("scenario.hrl").
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
            Dest = eliot_api:ip_to_node(Company),
            {company, Dest} ~ <<?CONSUMPTION:8/unsigned-little-integer, 100:16/unsigned-little-integer>>,
            erlang:send_after(?TIMER, self(), send);
        Any ->
            io:format("Sm Current: Unknown message ~p~n", [Any])
    end,
    current(State).
