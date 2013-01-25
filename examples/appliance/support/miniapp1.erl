-module(miniapp1).
-export([start/0]).
-record(state, {}).
-define(EVAL, $E).

% Public API
start() ->
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
