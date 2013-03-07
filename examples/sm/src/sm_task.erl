-module(sm_task).
-export([start_link/0, sm/0, schedule/0, get_appliances/0, set_appliances/1, test1/0, test2/0, test3/0, test4/0, reset/0]).
-include("scenario.hrl").
-include("eliot.hrl").
-define(TIMER, 10 * 1000).
-define(FNAME, "/home/crest/tests-eliot.txt").
-define(NAME, 'minsm_algorithm').
-record(state, {company = none, appliances = dict:new(), slots = [], cap = 0, sw = none, cur = 0}).

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
    SW2 = clocks:start(clock_gettime),
    SW3 = clocks:start(clock),
    SW4 = clocks:start(times),
    {ok, Dev} = application:get_env(sm, logger),
    lists:foldl(fun(_, Idx) ->
                        W1 = clocks:start(clock),
                        W2 = clocks:start(times),
                        W3 = clocks:start(clock_gettime),
                        WW1 = clocks:acc_start(W1),
                        WW2 = clocks:acc_start(W2),
                        WW3 = clocks:acc_start(W3),
                        Res = if
                            Idx == 0 ->
                                application:set_env(sm, sw11, WW1),
                                application:set_env(sm, sw21, WW2),
                                application:set_env(sm, sw31, WW3),
                                1;
                            true ->
                                application:set_env(sm, sw12, WW1),
                                application:set_env(sm, sw22, WW2),
                                application:set_env(sm, sw32, WW3),
                                0
                        end,
                        lists:foreach(fun(_) ->
                                                    sm ! beacon,
                                                    timer:sleep(?TIMER) end, lists:seq(1, 6)),
                        sm ! {schedule, Idx},
                        Res end, 0, lists:seq(1, 60)),
    NW2 = clocks:update(SW2),
    NW3 = clocks:update(SW3),
    NW4 = clocks:update(SW4),
    io:format(Dev, "WALL~c~p~n", [9, NW2#stopwatch.cur]),
    io:format(Dev, "CLOCK~c~p~n", [9, NW3#stopwatch.cur]),
    io:format(Dev, "TIMES~c~p~n", [9, NW4#stopwatch.cur]),
    io:format(Dev, "-------------------------------------------------------~n", []),
    file:close(Dev),
    reset(), % send the reset...
    timer:sleep(5), % wait for it...
    init:stop(). % quit.

test2() ->
    {ok, Dev} = file:open(?FNAME, [append]),
    lists:foreach(fun(_) ->
                          timer:sleep(?TIMER),
                          Time = unixtime:clock_gettime(),
                          io:format(Dev, "START~c~p~n", [9, Time]),
                          sm ! beacon,
                          timer:sleep(?TIMER),
                          reset(),
                          code:purge(?NAME),
                          code:delete(?NAME) end, lists:seq(1, 60)),
    file:close(Dev),
    init:stop().

test3() ->
    lists:foreach(fun(_) ->
                        lists:foreach(fun(_) ->
                                                    sm ! beacon,
                                                    timer:sleep(?TIMER) end, lists:seq(1, 6)),
                        sm ! schedule end, lists:seq(1, 2)),
    reset(), % send the reset...
    timer:sleep(5), % wait for it...
    init:stop(). % quit.

test4() ->
    {ok, Dev} = file:open(?FNAME, [append]),
    application:set_env(sm, logger, Dev),
    lists:foldl(fun(_, Idx) ->
                        W2 = clocks:start(times),
                        WW2 = clocks:acc_start(W2),
                        Res = if
                            Idx == 0 ->
                                application:set_env(sm, sw21, WW2),
                                1;
                            true ->
                                application:set_env(sm, sw22, WW2),
                                0
                        end,
                        lists:foreach(fun(_) ->
                                                    timer:sleep(?TIMER),
                                                    sm ! beacon end, lists:seq(1, 6)),
                        sm ! {schedule, Idx},
                        Res end, 0, lists:seq(1, 300)),
    io:format(Dev, "-------------------------------------------------------~n", []),
    file:close(Dev),
    reset(), % send the reset...
    timer:sleep(5), % wait for it...
    init:stop(). % quit.

% Private API

sm(#state{company = Company, appliances = Appliances, slots = Slots, cap = Cap, sw = SW, cur = Cur} = State) ->
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
            %SW2 = clocks:acc_start(SW),
            Pid = spawn_link(fun() -> sm_algorithm:schedule(#billing{slots = Slots, cap = Cap}, Appliances) end),
            register(alg, Pid),
            erlang:export(alg),
            sm(State);
        {schedule, Cur2} ->
            Pid = spawn_link(fun() -> sm_algorithm:schedule(#billing{slots = Slots, cap = Cap}, Appliances) end),
            register(alg, Pid),
            erlang:export(alg),
            sm(State#state{cur = Cur2});
        reset ->
            Msg = <<?RESET:8/unsigned-little-integer>>,
            {sm, all} ~ Msg,
            sm(State);
        {result, Schedule} ->
            sm_algorithm:notify(Schedule),
            %SW2 = clocks:acc_stop(SW),
            %{ok, W1} = if
            %    Cur == 0 ->
            %         application:get_env(sm, sw11);
            %    true ->
            %        application:get_env(sm, sw12)
            %end,
            {ok, W2} = if
                Cur == 0 ->
                     application:get_env(sm, sw21);
                true ->
                    application:get_env(sm, sw22)
            end,
            %{ok, W3} = if
            %    Cur == 0 ->
            %         application:get_env(sm, sw31);
            %    true ->
            %        application:get_env(sm, sw32)
            %end,
            %WW1 = clocks:acc_stop(W1),
            WW2 = clocks:acc_stop(W2),
            %WW3 = clocks:acc_stop(W3),
            {ok, Dev} = application:get_env(sm, logger),
            %io:format(Dev, "CLOCK~c~p~n", [9, WW1#stopwatch.last]),
            io:format(Dev, "TIMES~c~p~n", [9, WW2#stopwatch.last]),
            %io:format(Dev, "WALL~c~p~n", [9, WW3#stopwatch.last]),
            reset(),
            sm(State#state{company = Company, appliances = Schedule});
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
