-module(appliance_task).
-export([start_link/0, appliance/0]).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, appliance, []),
    register(appliance, Pid),
    erlang:export(appliance),
    {ok, Pid}.

appliance() ->
    % Task implementation goes here
    ok.
