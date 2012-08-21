-module(sm_task).
-export([start_link/0, sm/0]).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, sm, []),
    register(sm, Pid),
    erlang:export(sm),
    {ok, Pid}.

sm() ->
    % Task implementation goes here
    ok.
