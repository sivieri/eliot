-module({{appid}}_task).
-export([start_link/0, {{appid}}/0]).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, {{appid}}, []),
    register({{appid}}, Pid),
    erlang:export({{appid}}),
    {ok, Pid}.

{{appid}}() ->
    % Task implementation goes here
    ok.
