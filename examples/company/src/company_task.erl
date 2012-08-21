-module(company_task).
-export([start_link/0, company/0]).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, company, []),
    register(company, Pid),
    erlang:export(company),
    {ok, Pid}.

company() ->
    % Task implementation goes here
    ok.
