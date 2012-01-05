%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Dispatcher.
-module(wsn_dispatcher).
-export([start_link/0, loop/0]).

% Public API

start_link() ->
	Pid = spawn_link(?MODULE, loop, []),
	register(wsn_dispatcher, Pid),
	{ok, Pid}.

loop() ->
	receive
		{connect, all, Msg} ->
			lists:foreach(fun(Subject) -> Subject ! Msg end, wsn_export:get_exported());
		{connect, Subject, Msg} ->
			case wsn_export:is_exported(Subject) of
				true ->
					Subject ! Msg;
				false ->
					ok
			end,
			loop();
        {simulation, all, Msg} ->
            lists:foreach(fun(Subject) -> Subject ! Msg end, wsn_export:get_exported());
        {simulation, {Subject, Node}, Msg} ->
            QName = wsn_simulator:get_simname(Subject, Node),
            case wsn_export:is_exported(QName) of
                true ->
                    QName ! Msg;
                false ->
                    ok
            end,
            loop();
        {simulation, Subject, Msg} ->
            lists:foreach(fun(Elem) -> Elem ! Msg end, wsn_export:get_exported(Subject)),
            loop();
		Any ->
			io:format("WSN dispatcher: unable to parse ~p~n", [Any]),
			loop()
	end.

% Private API
