-module(ctp_task).
-export([start_link/0, collect/1, collect_test/2]).

% Public API

start_link() ->
    Pid = spawn_link(eliot_ctp, ctp, []),
    register(ctp, Pid),
    erlang:export(ctp),
    {ok, Pid}.

collect(Data) ->
    eliot_ctp:collect(ctp, Data).

collect_test(DestId, Data) ->
    {_NodeName, NodeAddress} = utils:split_name(node()),
    eliot_api:send_test(ctp, {DestId, NodeAddress}, {collect, Data}),
    ok.

% Private API
