-module(sm_stuff).
-export([start_link/0]).
-record(state, {}).
-include("eliot.hrl").
-include("scenario.hrl").

% Public API
start_link() ->
    Pid = spawn(fun() -> register_myself(), loop(#state{}) end),
    {ok, Pid}.

% Private API

loop(State) ->
    receive
        {Pid, data} ->
            Pid ! [#parameter{name = starttime, type = ?PARAM_TIME}, #parameter{name = endtime, type = ?PARAM_TIME, value = 1}],
            loop(State);
        {Pid, <<?EVAL:8/unsigned-little-integer, CurrentTime:8/unsigned-little-integer, Other/binary>>} ->
            Params = data:decode_params(Other),
            Ans = model(CurrentTime, Params),
            Pid ! <<?EVAL:8/unsigned-little-integer, Ans:16/unsigned-little-integer>>,
            loop(State);
        Any ->
            loop(State)
    end.

model(_Cur, _Params) ->
    1000.

register_myself() ->
    {ok, Dev} = application:get_env(sm, logger),
    {ok, SW} = application:get_env(sm, sw),
    SW2 = clocks:acc_stop(SW).
