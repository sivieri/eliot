%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc GPIO interface.
-module(eliot_gpio).
-behaviour(gen_server).
-export([start_link/0, dirin/1, dirout/1, get/1, set/2, clear/1]).
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

clear(Pin) ->
    gen_server:cast(?MODULE, {clear, Pin}).

init([]) ->
    {ok, #state{port = create_port()}}.

handle_call({get, Pin}, _From, #state{port = Port} = State) ->
    Port ! {self(), {command, term_to_binary({get, Pin})}},
    receive
        {Port, {data, Data}} ->
            {reply, binary_to_term(Data), State}
    end.

handle_cast({direction, Direction}, #state{port = Port} = State) ->
    Port ! {self(), {command, term_to_binary(Direction)}},
    {noreply, State};
handle_cast({set, Pin, Value}, #state{port = Port} = State) ->
    Port ! {self(), {command, term_to_binary({set, Pin, Value})}},
    {noreply, State};
handle_cast({clear, Pin}, #state{port = Port} = State) ->
    Port ! {self(), {command, term_to_binary({clear, Pin})}},
    {noreply, State}.

handle_info({Port, {exit_status, Status}}, #state{port = Port} = State) ->
    io:format("GPIO port exited with status ~p~n", [Status]),
    {noreply, State#state{port = create_port()}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API

create_port() ->
    case code:priv_dir(eliot) of
        {error, _} ->
            io:format("~w priv directory not found!", [eliot]),
            exit(error);
        PrivDir ->
            open_port({spawn, filename:join([PrivDir, "eliot_gpio"])}, [binary, {packet, 4}, exit_status])
    end.
