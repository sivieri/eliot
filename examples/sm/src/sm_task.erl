-module(sm_task).
-export([start_link/0, sm/0]).
-include("scenario.hrl").
-define(TIMER, 10 * 1000).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, sm, []),
    register(sm, Pid),
    erlang:export(oppflooder),
    erlang:export(sm),
    erlang:send_after(?TIMER, Pid, beacon),
    {ok, Pid}.

sm() ->
    receive
        beacon ->
            Id = eliot_api:nodeid(eliot_api:get_node_name()),
            Content = erlang:term_to_binary(sm),
            Msg = <<Id:?SRCADDR, Content>>,
            eliot_oppflooder:send(oppflooder, Msg);
        Any ->
            io:format("SM: Unknown message ~p~n", [Any])
    end,
    sm().
