-module(udp).

-export([listen/1, connect/1, accept/1, send/2, recv/1, close/1,
	 get_port/1, get_status_counters/1, controlling_process/2,
	 tick/1, get_creation/1, start/1, set_mode/2, print_ports/1, beacon_create/0, beacon/1,
     clean_create/0, clean/1]).
-include("dist_util.hrl").
-define(decode(A,B,C,D), (((A) bsl 24) bor 
			  ((B) bsl 16) bor ((C) bsl 8) bor (D))).
-define(encode(N), [(((N) bsr 24) band 16#FF), (((N) bsr 16) band 16#FF),  
		    (((N) bsr 8) band 16#FF), ((N) band 16#FF)]).  
-define(check_server(), case whereis(udp_server) of 
			    undefined ->
				exit(udp_server_not_started);
			    _ ->
				ok
			end).

listen(Name) ->
    ?check_server(),
    ?trace("DEBUG: Listening ~p...~n", [Name]),
    command(port(),$L,Name).

connect(Address) when is_tuple(Address) ->
    ?check_server(),
    AddrName = inet_parse:ntoa(Address),
    ?trace("DEBUG: Connecting ~p...~n", [AddrName]),
    command(port(),$C,AddrName);
connect(Address) ->
    ?check_server(),
    ?trace("DEBUG: Connecting ~p...~n", [Address]),
    command(port(),$C,Address).

accept(_Port) ->
    ?check_server(),
    ?trace("DEBUG: Accepting...~n", []),
    command(port(),$A,[]).

beacon_create() ->
    ?check_server(),
    ?trace("DEBUG: Creating beacon port...~n", []),
    port().

beacon(Port) ->
    ?check_server(),
    ?trace("DEBUG: Beaconing...~n", []),
    command(Port, $B, []).

clean_create() ->
    ?check_server(),
    ?trace("DEBUG: Creating cleaning port...~n", []),
    port().

clean(Port) ->
    ?check_server(),
    ?trace("DEBUG: Cleaning...~n", []),
    command(Port, $E, []).

send(Port,Data) ->
    ?check_server(),
    ?trace("DEBUG: Sending  ~p...~n", [Data]),
    command(Port, $S, Data).

recv(Port) ->
    ?check_server(),
    ?trace("DEBUG: Receiving...~n", []),
    command(Port, $R, []).

close(Port) ->
    ?check_server(),
    ?trace("DEBUG: Closing...~n", []),
    (catch unlink(Port)), %% Avoids problem with trap exits.
    case (catch erlang:port_close(Port)) of
	{'EXIT', _Reason} ->
	    {error, closed};
	_ ->
	    ok
    end.

get_port(Port) ->
    ?check_server(),
    ?trace("DEBUG: Getting port...~n", []),
    {ok,Port}.

get_status_counters(Port) ->
    ?check_server(),
    ?trace("DEBUG: Getting counters...~n", []),
    case control(Port, $S) of
	{ok, {C0, C1, C2}} ->
	    {ok, C0, C1, C2};
	Other ->
	    Other
    end.

get_creation(Port) ->
    ?check_server(),
    ?trace("DEBUG: Getting creation...~n", []),
    case control(Port, $R) of
	{ok, [A]} ->
	    A;
	Else ->
	    Else
    end.

tick(Port) ->
    ?check_server(),
    ?trace("DEBUG: Tick...~n", []),
    control(Port,$T).

set_mode(Port,data) ->
    ?check_server(),
    control(Port,$D).

print_ports(Port) ->
    ?check_server(),
    control(Port, $P).

controlling_process(Port, Pid) ->
    ?check_server(),
    case (catch erlang:port_connect(Port, Pid)) of
	true ->
	    (catch unlink(Port)),
	    ok;
	{'EXIT', {badarg, _}} ->
	    {error, closed};
	Else ->
	    exit({unexpected_driver_response, Else})
    end.

control(Port, Command) ->
    case (catch erlang:port_control(Port, Command, [])) of
	[0] ->
	    ok;
	[0,A] ->
	    {ok, [A]};
	[0,A,B,C,D] ->
	    {ok, [A,B,C,D]};
	[0,A1,B1,C1,D1,A2,B2,C2,D2,A3,B3,C3,D3] ->
	    {ok, {?decode(A1,B1,C1,D1),?decode(A2,B2,C2,D2),
		  ?decode(A3,B3,C3,D3)}};
	[1|Error] ->
	    exit({error, list_to_atom(Error)});
	{'EXIT', {badarg, _}} ->
	    {error, closed};
	Else ->
	    exit({unexpected_driver_response, Else})
    end.

command(Port, Command, Parameters) ->
    SavedTrapExit = process_flag(trap_exit,true),
    case (catch erlang:port_command(Port,[Command | Parameters])) of
	true ->
	    receive
		{Port, {data, [Command, $o, $k]}} ->
            ?trace("DEBUG: ~p ok~n", [Command]),
		    process_flag(trap_exit,SavedTrapExit),
		    {ok, Port};
		{Port, {data, [Command |T]}} ->
            ?trace("DEBUG: Received ~p:~p~n", [Command, T]),
		    process_flag(trap_exit,SavedTrapExit),
		    {ok, T};
		{Port, Else} ->
            ?trace("DEBUG: Unexpected response~n", []),
		    process_flag(trap_exit,SavedTrapExit),
		    exit({unexpected_driver_response, Else});
		{'EXIT', Port, normal} ->
            ?trace("DEBUG: Normal exit~n", []),
		    process_flag(trap_exit,SavedTrapExit),
		    {error, closed};
		{'EXIT', Port, Error} ->
            ?trace("DEBUG: Error exit~n", []),
		    process_flag(trap_exit,SavedTrapExit),
		    exit(Error)
	    end;
	{'EXIT', {badarg, _}} ->
        ?trace("DEBUG: Badarg~n", []),
	    process_flag(trap_exit,SavedTrapExit),
	    {error, closed};
	Unexpected ->
        ?trace("DEBUG: Unexpected response 2~n", []),
	    process_flag(trap_exit,SavedTrapExit),
	    exit({unexpected_driver_response, Unexpected})
    end.

port() ->
    SavedTrapExit = process_flag(trap_exit,true),
    case open_port({spawn, "eliot_udp"},[]) of
	P when is_port(P) ->
	    process_flag(trap_exit,SavedTrapExit),
	    P;
	{'EXIT',Error} ->
	    process_flag(trap_exit,SavedTrapExit),
	    exit(Error);
	Else ->
	    process_flag(trap_exit,SavedTrapExit),
	    exit({unexpected_driver_response, Else})
    end.

start(Name) ->
    net_kernel:start([Name, longnames]),
    erlang:set_cookie(node(), 'abc').
