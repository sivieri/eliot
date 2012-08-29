-module(sm_sup).
-behaviour(supervisor).
-export([start_link/0, add_child/2, stop_child/1]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILDARGS(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

% Public API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(eliot_oppflooder, worker),
                                  ?CHILD(sm_task, worker)]} }.

add_child(Child, Args) ->
    ChildSpec = ?CHILDARGS(Child, worker, Args),
    supervisor:start_child(?MODULE, ChildSpec).

stop_child(Child) ->
    supervisor:terminate_child(?MODULE, Child),
    supervisor:delete_child(?MODULE, Child).

% Private API
