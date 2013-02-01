-module(sm_task).
-export([start_link/0, sm/0, schedule/1, schedule/2, get_appliances/0, set_appliances/1, test_schedule/0, test1/0]).
-include("scenario.hrl").
-define(TIMER, 10 * 1000).
-record(state, {company = none, appliances = dict:new(), slots = [], cap = 0}).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, sm, []),
    register(sm, Pid),
    erlang:export(sm),
    %erlang:send_after(?TIMER, Pid, beacon),
    {ok, Pid}.

sm() ->
    sm(#state{}).

test_schedule() ->
    schedule([#slot{starttime = {7, 00}, endtime = {20, 00}, priority = 0},
                     #slot{starttime = {20, 00}, endtime = {7, 00}, priority = 1}]).

schedule(Slots) ->
    sm ! {schedule, Slots}.

schedule(Slots, Cap) ->
    sm ! {schedule, Slots, Cap}.

get_appliances() ->
    eliot_api:lpc(sm, {get, appliances}).

set_appliances(Appliances) ->
    sm ! {set, appliances, Appliances}.

test1() ->
    timer:sleep(30),
    lists:foreach(fun(_) -> sm ! beacon, timer:sleep(10) end, [1, 2, 3]),
    test_schedule(),
    lists:foreach(fun(_) -> sm ! beacon, timer:sleep(10) end, [1, 2, 3]).

% Private API

sm(#state{company = Company, appliances = Appliances, slots = Slots, cap = Cap} = State) ->
    receive
        beacon ->
            Msg = <<?SM:8/unsigned-little-integer>>,
            {sm, all} ~ Msg,
            %erlang:send_after(?TIMER, self(), beacon),
            sm(State);
        {Pid, {get, appliances}} ->
            Pid ! {self(), Appliances},
            sm(State);
        {set, appliances, NewAppliances} ->
            sm(State#state{appliances = NewAppliances});
        {schedule, NewSlots} ->
            Pid = spawn_link(fun() -> sm_algorithm:schedule(#billing{slots = NewSlots, cap = Cap}, Appliances) end),
            register(alg, Pid),
            erlang:export(alg),
            sm(State#state{slots = NewSlots});
        {schedule, NewSlots, NewCap} ->
            Pid = spawn_link(fun() -> sm_algorithm:schedule(#billing{slots = NewSlots, cap = NewCap}, Appliances) end),
            register(alg, Pid),
            erlang:export(alg),
            sm(State#state{slots = NewSlots, cap = NewCap});
        {result, Schedule} ->
            sm_algorithm:notify(Schedule),
            sm(#state{company = Company, appliances = Schedule});
        {_RSSI, Source, Content} ->
            {NewCompany, NewAppliances, NewSlots, NewCap} = case Content of
                <<?COMPANY:8/unsigned-little-integer, CCap:16/unsigned-little-integer>> when Company == none ->
                    io:format("SM: Registered a new company ~p~n", [Source]),
                    sm_sup:add_child(sm_current, [Source]),
                    {Source, Appliances, Slots, CCap};
                <<?COMPANY:8/unsigned-little-integer, CCap:16/unsigned-little-integer>> when Company == Source andalso CCap == Cap ->
                    {Company, Appliances, Slots, Cap};
                <<?COMPANY:8/unsigned-little-integer, CCap:16/unsigned-little-integer>> when Company == Source ->
                    self() ! {schedule, Slots, CCap},
                    {Company, Appliances, Slots, CCap};
                <<?COMPANY:8/unsigned-little-integer, _CCap:16/unsigned-little-integer>> ->
                    io:format("SM: Already registered to the company ~p (new request from ~p)~n", [Company, Source]),
                    {Company, Appliances,  Slots, Cap};
                <<?APPLIANCE:8/unsigned-little-integer>> ->
                    case dict:is_key(Source, Appliances) of
                        true ->
                            {Company, Appliances,  Slots, Cap};
                        false ->
                            io:format("SM: Registered a new appliance ~p~n", [Source]),
                            {Company, dict:store(Source, #appliance{ip = Source}, Appliances), Slots, Cap}
                    end;
                <<?APPLIANCE:8/unsigned-little-integer, ParamsBin/binary>>  ->
                    Params = data:decode_params(ParamsBin),
                    case dict:is_key(Source, Appliances) of
                        true ->
                            {Company, dict:store(Source, #appliance{ip = Source, pid = none, params = Params}, Appliances),  Slots, Cap};
                        false ->
                            {Company, Appliances,  Slots, Cap}
                    end;
                <<?APPLIANCE_LOCAL:8/unsigned-little-integer, Hash:20/binary, L1:8, Name:L1/binary, Code/binary>> ->
                    case dict:is_key(Source, Appliances) of
                        false ->
                            io:format("SM: Registered a new local appliance ~p~n", [Source]),
                            {Pid, Params} = sm_sup:start_model(data:decode_name(Name), Code, Hash),
                            {Company, dict:store(Source, #appliance{ip = Source, pid = Pid, params = Params}, Appliances),  Slots, Cap};
                        true ->
                            {Company, Appliances,  Slots, Cap}
                    end;
                Any ->
                    io:format("SM: Unknown device ~p~n", [Any]),
                    {Company, Appliances,  Slots, Cap}
            end,
            sm(#state{company = NewCompany, appliances = NewAppliances, slots = NewSlots, cap = NewCap});
        Any ->
            io:format("SM: Unknown message ~p~n", [Any]),
            sm(State)
    end.
