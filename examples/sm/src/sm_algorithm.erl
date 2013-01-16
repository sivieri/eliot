-module(sm_algorithm).
-export([schedule/2, notify/1]).
-include("scenario.hrl").
-include("eliot.hrl").

% Public API

schedule(#billing{slots = Slots, cap = Cap} = Billing, Appliances) ->
    io:format("SM Algorithm: Billing ~p~n", [Billing]),
    io:format("SM Algorithm: Appliances~n"),
    utils:print_dict(Appliances),
    SortedSlots = lists:sort(fun(#slot{priority = Priority1},
                                                  #slot{priority = Priority2}) when Priority2 >= Priority1 -> true;
                                                 (_Slot1, _Slot2) -> false end , Slots),
    NewAppliances = calc(SortedSlots, Cap, Appliances),
    io:format("SM Algorithm: New schedule~n"),
    utils:print_dict(NewAppliances),
    sm ! {result, NewAppliances}.

notify(Schedule) ->
    dict:fold(fun(_Name, #appliance{name = _Name, ip = IP, pid = _Pid, params = Params}, _AccIn) ->
                                Dest = utils:join_name(?NODENAME, IP),
                                {appliance, Dest} ~ eliot_api:msg(term_to_binary({schedule, Params})) end, 0, Schedule).

% Private API

calc([], _Cap, Appliances) ->
    Appliances;
calc([#slot{starttime = Starttime, endtime = Endtime}|T], Cap, Appliances) ->
    NewAppliances = calc_slot(Starttime, Endtime, Cap, Appliances),
    calc(T, Cap, NewAppliances).

calc_slot({StartHour, _StartMinute}, {EndHour, _EndMinute}, Cap, Appliances) when StartHour > EndHour->
    calc_single_slot(StartHour, EndHour + 24, StartHour, Cap, Appliances);
calc_slot({StartHour, _StartMinute}, {EndHour, _EndMinute}, Cap, Appliances) ->
    calc_single_slot(StartHour, EndHour, StartHour, Cap, Appliances).

calc_single_slot(Start, End, Cur, Cap, Appliances) when Cur < End ->
    {NewAppliances, _Total} = dict:fold(fun(Key, Appliance, {CurAppliances, CurConsumption}) ->
                                                                {NewAppliance, NewVal} = calc_single_app(Cur, CurConsumption, Cap, Appliance),
                                                                {dict:store(Key, NewAppliance, CurAppliances), CurConsumption + NewVal} end, {dict:new(), 0}, Appliances),
    calc_single_slot(Start, End, Cur + 1, Cap, NewAppliances);
calc_single_slot(_Start, _End, _Cur, _Cap, Appliances) ->
    Appliances.

calc_single_app(Cur, CurConsumption, Cap, #appliance{pid = Dest, params = Params} = Appliance) ->
    #parameter{name = starttime, value = Start} = hd(lists:filter(fun(#parameter{name = starttime}) -> true;
                                                                                                                 (_Parameter) -> false end, Params)),
    #parameter{name = endtime, value = End} = hd(lists:filter(fun(#parameter{name = endtime}) -> true;
                                                                                                                 (_Parameter) -> false end, Params)),
    if
        Cur >= Start andalso Cur < End ->
            Bin1 = data:encode_params(Params),
            Message = <<?EVAL:8/unsigned-little-integer, Cur:8/unsigned-little-integer, Bin1/binary>>,
            case eliot_api:rpc_noacks(Dest, Message) of
                <<?EVAL:8/unsigned-little-integer, Consumption:16/unsigned-little-integer>> when Consumption + CurConsumption =< Cap ->
                    {Appliance, Consumption};
                _Other ->
                    NewParams = lists:map(fun(#parameter{name = starttime, value = Value} = Param) -> Param#parameter{value= Value + 1 rem 24};
                                                                  (#parameter{name = endtime, value = Value} = Param) -> Param#parameter{value= Value + 1 rem 24};
                                                                  (Param) -> Param end, Params),
                    {Appliance#appliance{params = NewParams}, 0}
            end;
        true ->
            {Appliance, 0}
    end.
