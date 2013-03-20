-module(sm_sup).
-behaviour(supervisor).
-export([start_link/0, add_child/2, stop_child/1, start_model/3]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILDARGS(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).
-define(CAZZILLO(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

% Public API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(sm_task, worker)]} }.

add_child(Child, Args) ->
    ChildSpec = ?CHILDARGS(Child, worker, Args),
    supervisor:start_child(?MODULE, ChildSpec).

stop_child(Child) ->
    supervisor:terminate_child(?MODULE, Child),
    supervisor:delete_child(?MODULE, Child).

start_model(ModuleAtom, ModuleRealBinary, ModuleHash) ->
    % First: load module (or update it)
    Filename = erlang:atom_to_list(ModuleAtom) ++ ".erl",
    case code:is_loaded(ModuleAtom) of
        {file, _} ->
            {_, OldModuleBinary, _} = code:get_object_code(ModuleAtom),
            OldHash = crypto:sha(OldModuleBinary),
            case OldHash == ModuleHash of
                true ->
                    already_loaded;
                false ->
                    code:load_binary(ModuleAtom, Filename, ModuleRealBinary),
                    ok_updated
            end;
        false ->
            code:load_binary(ModuleAtom, Filename, ModuleRealBinary),
            ok
    end,
    % Second: spawn a new (supervised) process
    {ok, Pid} = add_child(ModuleAtom, []),
    Pid.

% Private API
