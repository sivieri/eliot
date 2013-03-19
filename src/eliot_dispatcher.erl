%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Dispatcher.
-module(eliot_dispatcher).
-include("eliot.hrl").
-export([start_link/0, loop/0]).

% Public API

start_link() ->
	Pid = spawn_link(?MODULE, loop, []),
	register(dispatcher, Pid),
	{ok, Pid}.

loop() ->
	receive
        {simulation, all, Msg = {{SenderName, _SenderNode}, _Msg}} ->
            lists:foreach(fun(Subject) -> send_msg(SenderName, Subject, Msg) end, erlang:exported()), % TODO probably duplicated messages
            loop();
        {simulation, {Subject, _Node}, Msg = {{SenderName, _SenderNode}, _Msg}} ->
            case lists:member(Subject, erlang:exported()) of
                true ->
                    send_msg(SenderName, Subject, Msg);
                false ->
                    ok
            end,
            loop();
        {simulation, Subject, Msg = {{SenderName, _SenderNode}, _Msg}} ->
            ProcessList = lists:filter(fun(Element) when is_atom(Element) -> lists:prefix(erlang:atom_to_list(Subject), erlang:atom_to_list(Element))
                                                                                                                andalso length(erlang:atom_to_list(Element)) > length(erlang:atom_to_list(Subject));
                                                       (_Element) -> false end, erlang:exported()),
            lists:foreach(fun(Elem) -> send_msg(SenderName, Elem, Msg) end, ProcessList),
            loop();
        {spawn, Fun} ->
            erlang:spawn(fun() -> Fun() end),
            loop();
        {spawn, Fun, Condition} ->
            case Condition() of
                true ->
                    erlang:spawn(fun() -> Fun() end);
                _ ->
                    ok
            end,
            loop();
        {spawn, Module, Function, Args} ->
            erlang:spawn(Module, Function, Args),
            loop();
        {spawn, Module, Function, Args, Condition} ->
            case Condition() of
                true ->
                    erlang:spawn(Module, Function, Args);
                _ ->
                    ok
            end,
            loop();
		Any ->
			io:format("eliot dispatcher: unable to parse ~p~n", [Any]),
			loop()
	end.

% Private API

send_msg(Node1, Node2, Msg) when Node1 == test ->
    {Source, Payload} = Msg,
    Node2 ! {0, Source, Payload};
send_msg(Node1, Node2, Msg) ->
    case eliot_forwarder:get_gain(Node1, eliot_simulator:get_name(Node2)) of
        inf ->
            ok;
        RSSI ->
            {Source, Payload} = Msg,
            Node2 ! {RSSI, Source, Payload}
  end.
