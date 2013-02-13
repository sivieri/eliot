-module(sm_task).
-export([start_link/0, sm/0, schedule/0, get_appliances/0, set_appliances/1, test1/0, reset/0]).
-include("scenario.hrl").
-include("eliot.hrl").
-define(TIMER, 10 * 1000).
-define(FNAME, "/home/crest/tests.txt").
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

schedule() ->
    sm ! schedule.

get_appliances() ->
    eliot_api:lpc(sm, {get, appliances}).

set_appliances(Appliances) ->
    sm ! {set, appliances, Appliances}.

reset() ->
    sm ! reset.

test1() ->
    SW1 = clocks:start(clock_gettime),
    SW2 = clocks:start(clock_gettime),
    SW3 = clocks:start(clock),
    SW4 = clocks:start(times),
    SW5 = clocks:start(clock_gettime),
    application:set_env(sm, task, SW1),
    application:set_env(sm, alg, SW5),
    RW2 = lists:foldl(fun(_, W2) ->
                        TTNW2 = lists:foldl(fun(_, WW2) ->
                                                    TW2 = clocks:acc_start(WW2),
                                                    sm ! beacon,
                                                    TWW2 = clocks:acc_stop(TW2),
                                                    timer:sleep(?TIMER),
                                                    TWW2 end, W2, lists:seq(1, 6)),
                        TNW2 = clocks:acc_start(TTNW2),
                        sm ! schedule,
                        NW2 = clocks:acc_stop(TNW2),
                        NW2 end, SW2, lists:seq(1, 10)),
    NW3 = clocks:update(SW3),
    NW4 = clocks:update(SW4),
    {ok, NSW1} = application:get_env(sm, task),
    {ok, NSW5} = application:get_env(sm, alg),
    case file:open(?FNAME, [append]) of
        {ok, Dev} ->
            io:format(Dev, "ELIOT~cACC~c~p~n", [9, 9, RW2#stopwatch.acc + NSW1#stopwatch.acc + NSW5#stopwatch.acc]),
            io:format(Dev, "ELIOT~cCLOCK~c~p~n", [9, 9, NW3#stopwatch.cur]),
            io:format(Dev, "ELIOT~cTIMES~c~p~n", [9, 9, NW4#stopwatch.cur]),
            file:close(Dev);
        {error, Reason} ->
            io:format("Unable to append to file: ~p~n", [Reason])
    end,
    reset(), % send the reset...
    timer:sleep(5), % wait for it...
    init:stop(). % quit.

% Private API

sm(#state{company = Company, appliances = Appliances, slots = Slots, cap = Cap} = State) ->
    receive
        beacon ->
            {ok, SW} = application:get_env(sm, task),
            SW2 = clocks:acc_start(SW),
            Msg = <<?SM:8/unsigned-little-integer>>,
            {sm, all} ~ Msg,
            %erlang:send_after(?TIMER, self(), beacon),
            SW3 = clocks:acc_stop(SW2),
            application:set_env(sm, task, SW3),
            sm(State);
        {Pid, {get, appliances}} ->
            Pid ! {self(), Appliances},
            sm(State);
        {set, appliances, NewAppliances} ->
            sm(State#state{appliances = NewAppliances});
        schedule ->
            {ok, SW} = application:get_env(sm, task),
            SW2 = clocks:acc_start(SW),
            Pid = spawn_link(fun() -> sm_algorithm:schedule(#billing{slots = Slots, cap = Cap}, Appliances) end),
            register(alg, Pid),
            erlang:export(alg),
            SW3 = clocks:acc_stop(SW2),
            application:set_env(sm, task, SW3),
            sm(State);
        reset ->
            Msg = <<?RESET:8/unsigned-little-integer>>,
            {sm, all} ~ Msg,
            sm(State);
        {result, Schedule} ->
            {ok, SW} = application:get_env(sm, task),
            SW2 = clocks:acc_start(SW),
            sm_algorithm:notify(Schedule),
            SW3 = clocks:acc_stop(SW2),
            application:set_env(sm, task, SW3),
            sm(#state{company = Company, appliances = Schedule});
        {_RSSI, Source, Content} ->
            {NewCompany, NewAppliances, NewSlots, NewCap} = case Content of
                <<?COMPANY:8/unsigned-little-integer, CCap:16/unsigned-little-integer, Other/binary>> when Company == none ->
                    io:format("SM: Registered a new company ~p~n", [Source]),
                    sm_sup:add_child(sm_current, [Source]),
                    SSlots = data:decode_slots(Other),
                    {Source, Appliances, SSlots, CCap};
                <<?COMPANY:8/unsigned-little-integer, CCap:16/unsigned-little-integer, Other/binary>> when Company == Source ->
                    SSlots = data:decode_slots(Other),
                    self() ! {schedule, SSlots, CCap},
                    {Company, Appliances, SSlots, CCap};
                <<?COMPANY:8/unsigned-little-integer, _CCap:16/unsigned-little-integer, _Other/binary>> ->
                    io:format("SM: Already registered to the company ~p (new request from ~p)~n", [Company, Source]),
                    {Company, Appliances,  Slots, Cap};
                <<?APPLIANCE:8/unsigned-little-integer, ParamsBin/binary>>  ->
                    Params = data:decode_params(ParamsBin),
                    case dict:is_key(Source, Appliances) of
                        true ->
                            {Company, Appliances, Slots, Cap};
                        false ->
                            io:format("SM: Registered a new appliance ~p~n", [Source]),
                            {Company, dict:store(Source, #appliance{ip = Source, pid = none, params = Params}, Appliances), Slots, Cap}
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
