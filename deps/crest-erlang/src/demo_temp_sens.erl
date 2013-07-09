-module(demo_temp_sens).
-export([sens_mean/0, start_link/0]).

start_link() ->
    Pid = spawn(?MODULE, sens_mean,[]),
    register(sens_mean, Pid),
    erlang:export(sens_mean),
    {ok, Pid}.

sens_mean()->
	receive
		{Node, [{"REQmean", Num}]} ->
			io:fwrite("DEBUG: Richiesta Corretta ~p~n",[Num]),
			PRes2=demo_temp_fun:mean_fun(Num)/Num,
			{rec_mean,Node} ! {node(), [{"mean", PRes2}]},
			io:format("~p~n~p~n", [{rec_mean,Node}, {node(), [{"mean", PRes2}]}]),
			sens_mean();
		{Node, Other} ->
            		io:fwrite("ERROR: ~p - ~p richiesta errata al sensore! ~n", [Node, Other]),
            		sens_mean()
	end.
