-module(demo_temp).
-export([temp_mean/0, rec_mean/3]).

%% External API
%% application:start(crypto). application:start(crest). crest_operations:invoke_spawn("localhost", demo_temp, fun() -> demo_temp:temp_mean() end).

temp_mean() ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), ["Temperature Mean"]},
                temp_mean();
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), ["POST"]},
                temp_mean();
	    {Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"valori", "integer()"}]},
                temp_mean();
            {Pid, [{"valori", Val}]} ->
		{Num, _} = string:to_integer(Val),		
		PRes1=demo_temp_fun:mean_fun(Num)/Num,
		io:fwrite("MEAN Singola: ~p ~n", [PRes1]),

		Pidr = spawn(?MODULE, rec_mean, [Pid,PRes1,0]),
		register(rec_mean, Pidr),
		erlang:export(rec_mean),

		{sens_mean,all} ! {node(), [{"REQmean", Num}]},
					
                temp_mean();
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                temp_mean()
        end.

rec_mean(Pid,Mean,Count) ->	
	receive
		{Node, [{"mean", PRes2}]} ->
			io:fwrite("DEBUG:~p - ~p ricezione!~n", [Node,PRes2]),
			TMean=Mean,
			Mean=(TMean+PRes2)/2,
			C=Count+1,
			rec_mean(Pid,Mean,C);
		{Node, Other} ->
            		io:fwrite("ERROR: ~p ricezione errata! ~n", [Other]),
            		rec_mean(Pid,Mean,Count)
	after
		1000 ->
			Pid ! {self(), {"text/plain", crest_utils:format("Mean: ~p, Count: ~p", [Mean,Count])}}
	end.
