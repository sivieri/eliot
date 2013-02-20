-module(sm_task).
-export([start_link/0, sm/0, schedule/0, get_appliances/0, set_appliances/1, test1/0, reset/0]).
-include("scenario.hrl").
-include("eliot.hrl").
-define(TIMER, 10 * 1000).
-define(FNAME, "/home/crest/tests-eliot.txt").
-record(state, {company = none, appliances = dict:new(), slots = [], cap = 0, sw = none}).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, sm, []),
    register(sm, Pid),
    erlang:export(sm),
    %erlang:send_after(?TIMER, Pid, beacon),
    {ok, Pid}.

sm() ->
    SW = clocks:start(clock_gettime),
    sm(#state{sw = SW}).

schedule() ->
    sm ! schedule.

get_appliances() ->
    eliot_api:lpc(sm, {get, appliances}).

set_appliances(Appliances) ->
    sm ! {set, appliances, Appliances}.

reset() ->
    sm ! reset.

test1() ->
    {ok, Dev} = file:open(?FNAME, [append]),
    application:set_env(sm, logger, Dev),
    SW3 = clocks:start(clock),
    SW4 = clocks:start(times),
    {ok, Dev} = application:get_env(sm, logger),
    lists:foreach(fun(_) ->
                        W1 = clocks:start(clock),
                        W2 = clocks:start(times),
                        SW1 = clocks:acc_start(W1),
                        SW2 = clocks:acc_start(W2),
                        application:set_env(sm, sw1, SW1),
                        application:set_env(sm, sw2, SW2),
                        lists:foreach(fun(_) ->
                                                    sm ! beacon,
                                                    timer:sleep(?TIMER) end, lists:seq(1, 6)),
                        sm ! schedule end, lists:seq(1, 60)),
    NW3 = clocks:update(SW3),
    NW4 = clocks:update(SW4),
    io:format(Dev, "CLOCK~c~p~n", [9, NW3#stopwatch.cur]),
    io:format(Dev, "TIMES~c~p~n", [9, NW4#stopwatch.cur]),
    io:format(Dev, "-------------------------------------------------------~n", []),
    file:close(Dev),
    reset(), % send the reset...
    timer:sleep(5), % wait for it...
    init:stop(). % quit.

% Private API

sm(#state{company = Company, appliances = Appliances, slots = Slots, cap = Cap, sw = SW} = State) ->
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
        schedule ->
            SW2 = clocks:acc_start(SW),
            Pid = spawn_link(fun() -> sm_algorithm:schedule(#billing{slots = Slots, cap = Cap}, Appliances) end),
            register(alg, Pid),
            erlang:export(alg),
            sm(State#state{sw = SW2});
        reset ->
            Msg = <<?RESET:8/unsigned-little-integer>>,
            {sm, all} ~ Msg,
            sm(State);
        {result, Schedule} ->
            sm_algorithm:notify(Schedule),
            SW2 = clocks:acc_stop(SW),
            {ok, W1} = application:get_env(sm, sw1),
            {ok, W2} = application:get_env(sm, sw2),
            WW1 = clocks:acc_stop(W1),
            WW2 = clocks:acc_stop(W2),
            {ok, Dev} = application:get_env(sm, logger),
            io:format(Dev, "CLOCK~c~p~n", [9, WW1#stopwatch.last]),
            io:format(Dev, "TIMES~c~p~n", [9, WW2#stopwatch.last]),
            io:format(Dev, "SCHED~c~p~n", [9, SW2#stopwatch.last]),
            sm(State#state{company = Company, appliances = Schedule, sw = SW2});
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
            sm(State#state{company = NewCompany, appliances = NewAppliances, slots = NewSlots, cap = NewCap});
        Any ->
            io:format("SM: Unknown message ~p~n", [Any]),
            sm(State)
    end.
