-module(demo_temp_fun).
-export([mean_fun/1]).

mean_fun(0) ->
	0;
mean_fun(Num) when Num > 0 ->
	(random:uniform(40)+mean_fun(Num-1)).
