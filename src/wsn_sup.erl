%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Supervisor.
-module(wsn_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

% Public API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(wsn_export, worker),
								  ?CHILD(wsn_dispatcher, worker)]} }.

% Private API