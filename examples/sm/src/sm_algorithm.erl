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
    calc(SortedSlots, Cap, Appliances),
    sm ! {result, Appliances}.

notify(Schedule) ->
    dict:fold(fun(_Name, #appliance{name = _Name, ip = IP, pid = _Pid, params = Params}, _AccIn) ->
                                Dest = utils:join_name(?NODENAME, IP),
                                {appliance, Dest} ! {schedule, Params} end, 0, Schedule).

% Private API

calc([#slot{starttime = Starttime, endtime = Endtime}|T], Cap, Appliances) ->
    NewAppliances = calc_slot(Starttime, Endtime, Cap, Appliances),
    calc(T, Cap, NewAppliances).

calc_slot({StartHour, _StartMinute}, {EndHour, _EndMinute}, Cap, Appliances) when StartHour > EndHour->
    calc_single_slot(StartHour, EndHour + 24, StartHour, Cap, Appliances);
calc_slot({StartHour, _StartMinute}, {EndHour, _EndMinute}, Cap, Appliances) ->
    calc_single_slot(StartHour, EndHour, StartHour, Cap, Appliances).

calc_single_slot(Start, End, Cur, Cap, Appliances) when Cur < End ->
    {NewAppliances, Total} = dict:fold(fun(A, B, C) -> ok end, {dict:new(), 0}, Appliances),
    calc_single_slot(Start, End, Cur + 1, Cap, NewAppliances);
calc_single_slot(_Start, End, Cur, _Cap, Appliances) ->
    Appliances.

calc_single_app(Cur, Cap, #appliance{params = Params} = Appliance) ->
    #parameter{name = starttime, value = Start} = hd(lists:filter(fun(#parameter{name = starttime}) -> true;
                                                                                                                 (_Parameter) -> false end, Params)),
    #parameter{name = endtime, value = End} = hd(lists:filter(fun(#parameter{name = endtime}) -> true;
                                                                                                                 (_Parameter) -> false end, Params)),
    if
        Cur >= Start andalso Cur < End ->
            ok;
        true ->
            ok
    end.
