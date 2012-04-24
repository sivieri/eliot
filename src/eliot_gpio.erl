%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc GPIO interface.
-module(eliot_gpio).
-behaviour(gen_server).
-export([start_link/0, dirin/1, dirout/1, get/1, set/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {port}).

% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

dirin(Pin) ->
    gen_server:cast(?MODULE, {direction, {in, Pin}}).

dirout(Pin) ->
    gen_server:cast(?MODULE, {direction, {out, Pin}}).

get(Pin) ->
    gen_server:call(?MODULE, {get, Pin}).

set(Pin, Value) ->
    gen_server:cast(?MODULE, {set, Pin, Value}).

init([]) ->
    {ok, #state{port = create_port()}}.

handle_call({get, Pin}, _From, #state{port = Port} = State) ->
    Res = send_int(Port, {get, Pin}),
    {reply, Res, State}.

handle_cast({direction, Direction}, #state{port = Port} = State) ->
    send_int(Port, Direction),
    {noreply, State};
handle_cast({set, Pin, Value}, #state{port = Port} = State) ->
    send_int(Port, {set, Pin, Value}),
    {noreply, State}.

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    io:format("GPIO port exited with status ~p~n", [Status]),
    {noreply, State#state{port = create_port()}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API

-ifdef(simulation).
create_port() ->
    ok.
-else.
create_port() ->
    open_port({spawn, filename:join([filename:absname(""), "deps", "eliot", "priv", "eliot-gpio"])}, [binary, {packet, 4}, exit_status]).
-endif.

-ifdef(simulation).
send_int(_Port, Msg) ->
    case Msg of
        {get, _Pin} ->
            case random:uniform(2) of
                1 ->
                    {ok, low};
                2 ->
                    {ok, high}
            end;
        _ ->
            ok
    end.
-else.
send_int(Port, Msg) ->
    erlang:port_command(Port, term_to_binary(Msg)),
    receive
        {Port, {data, Data}} ->
            Res = binary_to_term(Data),
            case Res of
                {error, Error} ->
                    io:format("GPIO error: ~p~n", [Error]);
                _ ->
                    ok
            end,
            Res
    end.
-endif.
