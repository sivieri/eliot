-module(clocks).
-export([start/1, update/1]).
-type swtype() :: gettimeofday | clock_gettime | clock | times.
-record(stopwatch, {type = gettimeofday :: swtype(), start = none, cur = none}).
-export_type([swtype/0]).

% Public API

start(Type) ->
    Val = unixtime:Type(),
    #stopwatch{type = Type, start = Val}.

update(#stopwatch{type = clock, start = Start} = SW) ->
    Val = unixtime:clock(),
    SW#stopwatch{cur = Val - Start};
update(#stopwatch{type = Type, start = {S1, S2}} = SW) ->
    {Cur1, Cur2} = unixtime:Type(),
    SW#stopwatch{cur = {Cur1 - S1, Cur2 - S2}}.

% Private API
