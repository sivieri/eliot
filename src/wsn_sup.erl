%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Supervisor.
-module(wsn_sup).
-behaviour(supervisor).
-export([start_link/0, init/1, start_task/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

% Public API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(wsn_export, worker),
								  ?CHILD(wsn_dispatcher, worker),
                                  ?CHILD(wsn_ping, worker)]} }.

start_task(Module) ->
    supervisor:start_child(?MODULE, ?CHILD(Module, worker)).

% Private API
