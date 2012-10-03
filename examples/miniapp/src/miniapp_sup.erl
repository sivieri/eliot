-module(miniapp_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

% Public API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(eliot_oppflooder_event, worker),
                                                    ?CHILD(eliot_oppflooder, worker),
                                                    ?CHILD(miniapp_model, worker),
                                                    ?CHILD(miniapp_task, worker)]} }.

% Private API