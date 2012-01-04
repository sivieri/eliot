-module({{appid}}_task).
-export([start_link/0, {{appid}}/0]).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, {{appid}}, []),
    register({{appid}}, Pid),
    wsn_export:export({{appid}}),
    {ok, Pid}.

{{appid}}() ->
    ok.
