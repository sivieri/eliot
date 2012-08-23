-module(sm_algorithm).
-export([schedule/2, notify/1]).
-include("scenario.hrl").
-include("eliot.hrl").

% Public API

schedule(#billing{starttime = Starttime, endtime = Endtime, cap = Cap} = Billing, Appliances) ->
    io:format("SM Algorithm: Billing ~p~n", [Billing]),
    io:format("SM Algorithm: Appliances~n"),
    utils:print_dict(Appliances),
    sm ! {result, Appliances}.

notify(Schedule) ->
    dict:fold(fun(Name, #appliance{name = _Name, ip = IP, pid = _Pid, params = Params}, _AccIn) ->
                                Dest = utils:join_name(?NODENAME, IP),
                                {appliance, Dest} ! {schedule, Params} end, 0, Schedule).

% Private API


