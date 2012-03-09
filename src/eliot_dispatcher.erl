%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Dispatcher.
-module(eliot_dispatcher).
-export([start_link/0, loop/0]).

% Public API

start_link() ->
	Pid = spawn_link(?MODULE, loop, []),
	register(eliot_dispatcher, Pid),
	{ok, Pid}.

loop() ->
	receive
		{connect, all, Msg} ->
			lists:foreach(fun(Subject) -> Subject ! Msg end, eliot_export:get_exported()),
            loop();
		{connect, Subject, Msg} ->
			case eliot_export:is_exported(Subject) of
				true ->
					Subject ! {0, Msg};
				false ->
					ok
			end,
			loop();
        {simulation, all, Msg = {{SenderName, _SenderNode}, _Msg}} ->
            lists:foreach(fun(Subject) -> send_msg(SenderName, Subject, Msg) end, eliot_export:get_exported()),
            loop();
        {simulation, {Subject, Node}, Msg = {{SenderName, _SenderNode}, _Msg}} ->
            QName = eliot_simulator:get_simname(Subject, Node),
            case eliot_export:is_exported(QName) of
                true ->
                    send_msg(SenderName, QName, Msg);
                false ->
                    ok
            end,
            loop();
        {simulation, Subject, Msg = {{SenderName, _SenderNode}, _Msg}} ->
            lists:foreach(fun(Elem) -> send_msg(SenderName, Elem, Msg) end, eliot_export:get_exported(Subject)),
            loop();
		Any ->
			io:format("eliot dispatcher: unable to parse ~p~n", [Any]),
			loop()
	end.

% Private API

send_msg(Node1, Node2, Msg) ->
    case eliot_forwarder:get_gain(Node1, eliot_simulator:get_name(Node2)) of
        inf ->
            ok;
        RSSI ->
            Node2 ! {RSSI, Msg}
  end.