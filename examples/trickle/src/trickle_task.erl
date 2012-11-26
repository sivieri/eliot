-module(trickle_task).
-export([start_link/0, update_version/2, update_version_test/3]).
-define(SRCADDR, 16/unsigned-little-integer).
-define(VERSION, 32/unsigned-little-integer).

% Public API

-ifdef(simulation).
start_link() ->
    Pid = spawn_link(eliot_trickle, trickle, []),
    register(trickle, Pid),
    erlang:export(trickle),
    {ok, Pid}.
-else.
start_link() ->
    ignore.
-endif.

update_version(Version, Payload) ->
    eliot_trickle:update_version(trickle, Version, Payload).

update_version_test(DestId, Version, Payload) ->
    Id = eliot_api:nodeid(DestId),
    PayloadB = term_to_binary(Payload),
    Msg = <<Id:?SRCADDR, Version:?VERSION, PayloadB/binary>>,
    io:format(standard_error, "~p~n", [utils:split_name(node())]),
    {_NodeName, NodeAddress} = utils:split_name(node()),
    eliot_api:send_test(trickle, {DestId, NodeAddress}, {version, Msg}),
    ok.

% Private API
