-module(sm_task).
-export([start_link/0, sm/0, schedule/0, get_appliances/0, set_appliances/1, test1/0, test2/0, test3/0, test4/0, reset/0]).
-include("scenario.hrl").
-include("eliot.hrl").
-define(TIMER, 2 * 1000).
-define(FNAME, "tests-boot-eliot.txt").
-define(NAME, 'sm_stuff').
-define(DIVISION, 50).
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
    application:set_env(sm, logger, Dev),
    timer:sleep(?TIMER),
    SW = clocks:start(clock_gettime),
    SW2 = clocks:acc_start(SW),
    application:set_env(sm, sw, SW2),
    send_fun(),
    timer:sleep(?TIMER),
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
    SW1 = clocks:start(getrusage),
    lists:foldl(fun(I, {Idx1, Idx2}) ->
                        application:set_env(sm, index, I),
                        W2 = clocks:start(getrusage),
                        WW2 = clocks:acc_start(W2),
                        W3 = clocks:start(getrusage),
                        NewIdx1 = if
                            Idx1 == 0 ->
                                application:set_env(sm, sw21, WW2),
                                1;
                            true ->
                                application:set_env(sm, sw22, WW2),
                                0
                        end,
                        NewIdx2 = case {Idx2, I} of
                                      {0, N} when N rem ?DIVISION == 0 ->
                                          WW3 = clocks:acc_start(W3),
                                          application:set_env(sm, sw31, WW3),
                                          1;
                                      {0, _N} ->
                                          0;
                                      {1, N} when N rem ?DIVISION == 0 ->
                                          WW3 = clocks:acc_start(W3),
                                          application:set_env(sm, sw32, WW3),
                                          0;
                                      {1, _N} ->
                                          1
                        end,
                        lists:foreach(fun(_) ->
                                                    timer:sleep(?TIMER),
                                                    sm ! beacon end, lists:seq(1, 6)),
                        sm ! {schedule, {Idx1, Idx2}},
                        {NewIdx1, NewIdx2} end, {0,0}, lists:seq(1, 30 * ?DIVISION)),
    FinalSW1 = clocks:update(SW1),
    io:format(Dev, "FINAL~c~p~n", [9, FinalSW1#stopwatch.cur]),
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
            sm(State#state{company = none, appliances = dict:new(), slots = [], cap = 0});
        {result, Schedule} ->
            sm_algorithm:notify(Schedule),
            %SW2 = clocks:acc_stop(SW),
            %{ok, W1} = if
            %    Cur == 0 ->
            %         application:get_env(sm, sw11);
            %    true ->
            %        application:get_env(sm, sw12)
            %end,
            {Cur1, Cur2} = Cur,
            {ok, W2} = if
                Cur1 == 0 ->
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
            io:format(Dev, "GETRUSAGE~c~p~n", [9, WW2#stopwatch.last]),
            %io:format(Dev, "WALL~c~p~n", [9, WW3#stopwatch.last]),
            {ok, Index} = application:get_env(sm, index),
            if
                Index /= 0 andalso Index rem ?DIVISION == 0 ->
                    {ok, W3} = if
                        Cur2 == 0 ->
                             application:get_env(sm, sw31);
                        true ->
                            application:get_env(sm, sw32)
                    end,
                    WW3 = clocks:acc_stop(W3),
                    io:format(Dev, "GETRUSAGE50~c~p~n", [9, WW3#stopwatch.last]);
                true ->
                    ok
            end,
            reset(),
            sm(State#state{company = Company, appliances = Schedule});
        {_RSSI, Source, Content} ->
            {NewCompany, NewAppliances, NewSlots, NewCap} = case Content of
                <<?COMPANY:8/unsigned-little-integer, CCap:16/unsigned-little-integer, Other/binary>> when Company == none ->
                    sm_sup:add_child(sm_current, [Source]),
                    SSlots = data:decode_slots(Other),
                    {Source, Appliances, SSlots, CCap};
                <<?COMPANY:8/unsigned-little-integer, CCap:16/unsigned-little-integer, Other/binary>> when Company == Source ->
                    SSlots = data:decode_slots(Other),
                    self() ! {schedule, SSlots, CCap},
                    {Company, Appliances, SSlots, CCap};
                <<?COMPANY:8/unsigned-little-integer, _CCap:16/unsigned-little-integer, _Other/binary>> ->
                    {Company, Appliances,  Slots, Cap};
                <<?APPLIANCE:8/unsigned-little-integer, ParamsBin/binary>>  ->
                    Params = data:decode_params(ParamsBin),
                    case dict:is_key(Source, Appliances) of
                        true ->
                            {Company, Appliances, Slots, Cap};
                        false ->
                            {Company, dict:store(Source, #appliance{ip = Source, pid = none, params = Params}, Appliances), Slots, Cap}
                    end;
                <<?APPLIANCE_LOCAL:8/unsigned-little-integer, Hash:20/binary, L1:8, Name:L1/binary, Code/binary>> ->
                    case dict:is_key(Source, Appliances) of
                        false ->
                            Pid = sm_sup:start_model(data:decode_name(Name), Code, Hash),
                            {Company, dict:store(Source, #appliance{ip = Source, pid = Pid}, Appliances),  Slots, Cap};
                        true ->
                            {Company, Appliances,  Slots, Cap}
                    end;
                Any ->
                    {Company, Appliances,  Slots, Cap}
            end,
            sm(State#state{company = NewCompany, appliances = NewAppliances, slots = NewSlots, cap = NewCap});
        Any ->
            sm(State)
    end.

send_fun() ->
    Name = data:encode_name(?NAME),
    {_, ModuleBinary, _} = code:get_object_code(?NAME),
    Hash = crypto:sha(ModuleBinary),
    sm ! {0, {192,168,1,2}, <<?APPLIANCE_LOCAL:8/unsigned-little-integer, Hash:20/binary, Name/binary, ModuleBinary/binary>>}.
