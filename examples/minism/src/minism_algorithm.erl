-module(minism_algorithm).
-export([start_link/0]).
-record(state, {}).
-define(EVAL, $E).
-include("eliot.hrl").

% Public API
start_link() ->
    register_myself(),
    loop(#state{}).

% Private API

loop(State) ->
    receive
        {Pid, <<?EVAL:8/unsigned-little-integer, CurrentTime:8/unsigned-little-integer, Other/binary>>} ->
            Params = data:decode_params(Other),
            io:format("Appliance: Evaluation of parameters ~p at time ~p~n", [Params, CurrentTime]),
            Ans = model(CurrentTime, Params),
            Pid ! <<?EVAL:8/unsigned-little-integer, Ans:16/unsigned-little-integer>>,
            loop(State);
        Any ->
            io:format("Message unknown: ~p~n", [Any]),
            loop(State)
    end.

model(_Cur, _Params) ->
    1000.

register_myself() ->
    {ok, Dev} = application:get_env(sm, logger),
    {ok, SW} = application:get_env(sm, sw),
    SW2 = clocks:acc_stop(SW),
    io:format(Dev, "SLOT~c~p~n", [9, SW2#stopwatch.last]).
