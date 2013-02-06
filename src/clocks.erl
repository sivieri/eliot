-module(clocks).
-export([start/1, update/1]).
-type swtype() :: gettimeofday | clock_gettime | clock | times.
-record(stopwatch, {type = gettimeofday :: swtype(), start = none, cur = none}).
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

% Private API
