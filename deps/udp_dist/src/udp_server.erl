-module(udp_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(DRIVER_NAME,"eliot_udp").

% Public API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit,true),
    Res = load_driver(),
    case Res of
	ok ->
	    {ok, []};
	{error, already_loaded} ->
	    {ok, []};
	Error ->
	    exit(Error)
    end.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    erl_ddll:unload_driver(?DRIVER_NAME),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API

load_driver() ->
    %Dir = find_priv_lib(),
    erl_ddll:load_driver("/home/crest/eliot/deps/udp_dist/priv",?DRIVER_NAME).

find_priv_lib() ->
    case (catch code:priv_dir(udp_dist)) of
		  {error, _} ->
		      %% Code server probably not startet yet
		      {ok, P} = erl_prim_loader:get_path(),
		      ModuleFile = atom_to_list(?MODULE) ++ extension(),
		      Pd = (catch lists:foldl
			    (fun(X,Acc) ->
				     M = filename:join([X, ModuleFile]),
				     %% The file server probably not started
				     %% either, has to use raw interface.
				     case file:raw_read_file_info(M) of 
					 {ok,_} -> 
					     %% Found our own module in the
					     %% path, lets bail out with
					     %% the priv_dir of this directory
					     Y = filename:split(X),
					     throw(filename:join
						   (lists:sublist
						    (Y,length(Y) - 1) 
						    ++ ["priv"])); 
					 _ -> 
					     Acc 
				     end 
			     end,
			     false,P)),
		      case Pd of
			  false ->
			      exit(udp_dist_priv_lib_indeterminate);
			  _ ->
			      Pd
		      end;
		  Dir ->
		      Dir
	      end.

extension() ->
    %% erlang:info(machine) returns machine name as text in all uppercase
    "." ++ lists:map(fun(X) ->
			     X + $a - $A
		     end,
		     erlang:info(machine)).
