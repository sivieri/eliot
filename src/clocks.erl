-module(clocks).
-export([start/1, update/1, acc_start/1, acc_stop/1]).
-type swtype() :: gettimeofday | clock_gettime | clock | times.
-record(stopwatch, {type = gettimeofday :: swtype(), start = none, cur = none, startacc = none, acc = none}).
-export_type([swtype/0]).

% Public API

start(times) ->
    Val = unixtime:times(),
    #stopwatch{type = times, start = Val};
start(clock) ->
    Val = unixtime:clock(),
    #stopwatch{type = clock, start = Val};
start(gettimeofday) ->
    {Secs, USecs} = unixtime:gettimeofday(),
    Val = Secs * 1000000 + USecs,
    #stopwatch{type = gettimeofday, start = Val};
start(clock_gettime) ->
    {Secs, NSecs} = unixtime:clock_gettime(),
    Val = Secs * 1000000000 + NSecs,
    #stopwatch{type = clock_gettime, start = Val}.

update(#stopwatch{type = clock, start = Start} = SW) ->
    Val = unixtime:clock(),
    SW#stopwatch{cur = Val - Start};
update(#stopwatch{type = times, start = {S1, S2}} = SW) ->
    {Cur1, Cur2} = unixtime:times(),
    SW#stopwatch{cur = {Cur1 - S1, Cur2 - S2}};
update(#stopwatch{type = gettimeofday, start = Start} = SW) ->
    {Secs, USecs} = unixtime:gettimeofday(),
    Val = Secs * 1000000 + USecs,
    SW#stopwatch{cur = Val - Start};
update(#stopwatch{type = clock_gettime, start = Start} = SW) ->
    {Secs, NSecs} = unixtime:clock_gettime(),
    Val = Secs * 1000000000 + NSecs,
    SW#stopwatch{cur = Val - Start}.

acc_start(#stopwatch{type = times} = SW) ->
    Val = unixtime:times(),
    SW#stopwatch{startacc = Val, acc = {0, 0}};
acc_start(#stopwatch{type = clock} = SW) ->
    Val = unixtime:clock(),
    SW#stopwatch{startacc = Val, acc = 0};
acc_start(#stopwatch{type = gettimeofday} = SW) ->
    {Secs, USecs} = unixtime:gettimeofday(),
    Val = Secs * 1000000 + USecs,
    SW#stopwatch{startacc = Val, acc = 0};
acc_start(#stopwatch{type = clock_gettime} = SW) ->
    {Secs, NSecs} = unixtime:clock_gettime(),
    Val = Secs * 1000000000 + NSecs,
    SW#stopwatch{startacc = Val, acc = 0}.

acc_stop(#stopwatch{type = clock, startacc = Start, acc = Acc} = SW) ->
    Val = unixtime:clock(),
    SW#stopwatch{acc = Acc + Val - Start};
acc_stop(#stopwatch{type = times, startacc = {S1, S2}, acc = {Acc1, Acc2}} = SW) ->
    {Cur1, Cur2} = unixtime:times(),
    SW#stopwatch{acc = {Acc1 + Cur1 - S1, Acc2 + Cur2 - S2}};
acc_stop(#stopwatch{type = gettimeofday, startacc = Start, acc = Acc} = SW) ->
    {Secs, USecs} = unixtime:gettimeofday(),
    Val = Secs * 1000000 + USecs,
    SW#stopwatch{acc = Acc + Val - Start};
acc_stop(#stopwatch{type = clock_gettime, startacc = Start, acc = Acc} = SW) ->
    {Secs, NSecs} = unixtime:clock_gettime(),
    Val = Secs * 1000000000 + NSecs,
    SW#stopwatch{acc = Acc + Val - Start}.

% Private API
