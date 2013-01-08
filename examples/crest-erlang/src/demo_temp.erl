-module(demo_temp).
-export([temp_mean/0]).

%% External API
%% application:start(crypto). application:start(crest). crest_operations:invoke_spawn("localhost", demo_temp, fun() -> demo_temp:temp_mean() end).


temp_mean() ->
        receive
            {Pid, {"param", "name"}} ->
                Pid ! {self(), "temperature mean"},
                temp_mean();
            {Pid, {"param", "operation"}} ->
                Pid ! {self(), "POST"},
                temp_mean();
	    {Pid, {"param", "parameters"}} ->
                Pid ! {self(), [{"valori", "integer()"}]}, %%{"address", "string()"}, 
                temp_mean();
            {Pid, [{"valori", Val}]} -> %%{"address", Address},
		{Num, _} = string:to_integer(Val),		%%Pid ! {self(), {"text/plain", crest_utils:format("Num: ~p", [Num])}},		
		Res=demo_temp_fun:mean_fun(Num)/Num,
                Pid ! {self(), {"text/plain", crest_utils:format("Mean: ~p", [Res])}},
                temp_mean();
            {Pid, Other} ->
                Pid ! {self(), {"text/plain", crest_utils:format("Error: ~p", [Other])}},
                temp_mean()
        end.





