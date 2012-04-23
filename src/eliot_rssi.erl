%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc RSSI producer.
-module(eliot_rssi).
-behaviour(gen_server).
-export([start_link/0, rssi/1, rssi/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {port}).

% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

rssi(Interface) ->
    gen_server:call(?MODULE, {all, Interface}).

rssi(Interface, Client) ->
    gen_server:call(?MODULE, {single, Interface, Client}).

init([]) ->
    {ok, #state{port = create_port()}}.

handle_call({all, Interface}, _From, #state{port = Port} = State) ->
    Res = send_int(Port, {scan, Interface}),
    {reply, Res, State};
handle_call({single, Interface, Client}, _From, #state{port = Port} = State) ->
    {ok, List} = send_int(Port, {scan, Interface}),
    Res = lists:foldl(fun({Addr, _RSSI}, _AccIn) when Addr == Client -> Client;
                                   (_Tuple, AccIn) -> AccIn end, {}, List),
    {reply, {ok, Res}, State}.

handle_cast(_Msg, State) ->
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
    case code:priv_dir(eliot) of
        {error, _} ->
            io:format("~w priv directory not found!", [eliot]),
            exit(error);
        PrivDir ->
            open_port({spawn, filename:join([PrivDir, "eliot-rssi"])}, [binary, {packet, 4}, exit_status])
    end.
-endif.

-ifdef(simulation).
send_int(_Port, _Msg) ->
    ok. % This should never be invoked: the simulation module gets the correct values
-else.
send_int(Port, Msg) ->
    erlang:port_command(Port, term_to_binary(Msg)),
    receive
        {Port, {data, Data}} ->
            Res = binary_to_term(Data),
            case Res of
                {error, Error} ->
                    io:format("RSSI error: ~p~n", [Error]);
                _ ->
                    ok
            end,
            Res
    end.
-endif.
