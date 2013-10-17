-module(firmata).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1, digital_read/1, analog_read/1, pin_mode/2, digital_write/2, analog_write/2]).
-define(SPEED, 57600).
-record(state, {device = none, analog = none, digital = none, output = none, bytes = <<>>}).

% Public API

start_link(Device) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Device], []).

digital_read(Pin) ->
    gen_server:call(?MODULE, {digital, read, Pin}).

analog_read(Pin) ->
    gen_server:call(?MODULE, {analog, read, Pin}).

pin_mode(Pin, Mode) ->
    gen_server:cast(?MODULE, {mode, Pin, Mode}).

digital_write(Pin, Value) ->
    gen_server:cast(?MODULE, {digital, write, Pin, Value}).

analog_write(Pin, Value) ->
    gen_server:cast(?MODULE, {analog, write, Pin, Value}).

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
init([Device]) ->
    SerialPort = serial:start([{speed, ?SPEED}, {open, Device}]),
    Analog = lists:foldl(fun(I, AccIn) -> dict:store(I, 0, AccIn) end, dict:new(), lists:seq(0, 15)),
    Digital = lists:foldl(fun(I, AccIn) -> dict:store(I, 0, AccIn) end, dict:new(), lists:seq(0, 15)),
    Output = lists:foldl(fun(I, AccIn) -> dict:store(I, 0, AccIn) end, dict:new(), lists:seq(0, 15)),
    {ok, #state{device = SerialPort, analog = Analog, digital = Digital, output = Output}}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
handle_call({digital, read, Pin}, _From, State = #state{digital = Digital}) ->
    Reply = dict:fetch(Pin, Digital),
    {reply, Reply, State};
handle_call({analog, read, Pin}, _From, State = #state{analog = Analog}) ->
    Reply = dict:fetch(Pin, Analog),
    {reply, Reply, State};
handle_call(Request, _From, State) ->
    Reply = error,
    io:format(standard_error, "Unknown CALL message ~p~n", [Request]),
    {reply, Reply, State}.

-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_cast({pin, Pin, Mode}, State = #state{device = Device}) ->
    Device ! {send, <<244:8/integer, Pin:8/integer, Mode:8/integer>>},
    {noreply, State};
handle_cast({digital, write, Pin, Value}, State = #state{device = Device, output = Output}) ->
    PortNumber = (pin bsr 3) band 15,
    Current = dict:fetch(PortNumber, Output),
    NewCurrent = case Value of
        0 ->
            Current band (bnot (1 bsl (Pin band 7)));
        1 ->
            Current bor (bnot (1 bsl (Pin band 7)))
    end,
    NewOutput = dict:store(PortNumber, NewCurrent, Output),
    Cmd = 144 bor (PortNumber),
    First = NewCurrent band 127,
    Second = NewCurrent bsr 7,
    Device ! {send, <<Cmd:8/integer, First:8/integer, Second:8/integer>>},
    {noreply, State#state{output = NewOutput}};
handle_cast({analog, write, Pin, Value}, State = #state{device = Device}) ->
    Cmd = 224 bor (Pin band 15),
    First = Value band 127,
    Second = Value bsr 7,
    Device ! {send, <<Cmd:8/integer, First:8/integer, Second:8/integer>>},
    {noreply, State};
handle_cast(Msg, State) ->
    io:format(standard_error, "Unknown CAST message ~p~n", [Msg]),
    {noreply, State}.

-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_info({data, NewBytes}, State = #state{analog = Analog, digital = Digital, bytes = Bytes}) when byte_size(NewBytes) + byte_size(Bytes)  >= 3 ->
    {NewAnalog, NewDigital} = parse_msg(<<Bytes/binary, NewBytes/binary>>, Analog, Digital),
    {noreply, State#state{analog = NewAnalog, digital = NewDigital, bytes = <<>>}};
handle_info({data, NewBytes}, State) ->
    {noreply, State#state{bytes = NewBytes}};
handle_info(Info, State) ->
    io:format(standard_error, "Unknown INFO message ~p~n", [Info]),
    {noreply, State}.

-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API

parse_msg(<<>>, Analog, Digital) ->
    {Analog, Digital};
parse_msg(<<224:8/integer, Ch:8/integer, Lsb:8/integer, Msb:8/integer, Other/binary>>, Analog, Digital) ->
    parse_msg(Other, dict:store(Ch, (Msb bsl 7) + Lsb, Analog), Digital);
parse_msg(<<144:8/integer, Ch:8/integer, Lsb:8/integer, Msb:8/integer, Other/binary>>, Analog, Digital) ->
    parse_msg(Other, Analog, dict:store(Ch, (Msb bsl 7) + Lsb, Digital));
parse_msg(<<_Unknown:8/integer, Other/binary>>, Analog, Digital) ->
    parse_msg(Other, Analog, Digital).
