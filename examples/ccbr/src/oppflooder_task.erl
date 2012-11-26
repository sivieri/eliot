-module(oppflooder_task).
-export([start_link/0, send/1, send_test/2]).

% Public API

-ifdef(simulation).
start_link() ->
    eliot_oppflooder_event:start(),
    Pid = eliot_oppflooder:start_link(),
    oppflooder_handler:add_handler(),
    {ok, Pid}.
-else.
start_link() ->
    oppflooder_handler:add_handler(),
    ignore.
-endif.

send(Payload) ->
    eliot_oppflooder:send(oppflooder, Payload).

send_test(DestId, Payload) ->
    {_NodeName, NodeAddress} = utils:split_name(node()),
    eliot_api:send_test(oppflooder, {DestId, NodeAddress}, {send, Payload}),
    ok.

% Private API
