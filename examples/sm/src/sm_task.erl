-module(sm_task).
-export([start_link/0, sm/0, schedule/2, get_appliances/0, set_appliances/1]).
-include("scenario.hrl").
-define(TIMER, 10 * 1000).
-record(state, {company = none, appliances = dict:new(), sensors = [], slots = []}).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, sm, []),
    register(sm, Pid),
    erlang:export(sm),
    erlang:send_after(?TIMER, Pid, beacon),
    {ok, Pid}.

sm() ->
    sm(#state{}).

schedule(Slots, Cap) ->
    sm ! {schedule, Slots, Cap}.

get_appliances() ->
    eliot_api:lpc(sm, {get, appliances}).

set_appliances(Appliances) ->
    sm ! {set, appliances, Appliances}.

% Private API

sm(#state{company = Company, appliances = Appliances, sensors = Sensors, slots = Slots} = State) ->
    receive
        beacon ->
            Msg = erlang:term_to_binary(sm),
            eliot_oppflooder:send(oppflooder, Msg),
            erlang:send_after(?TIMER, self(), beacon),
            sm(State);
        {Pid, {get, appliances}} ->
            Pid ! {self(), Appliances},
            sm(State);
        {set, appliances, NewAppliances} ->
            sm(State#state{appliances = NewAppliances});
        {schedule, NewSlots, Cap} ->
            Pid = spawn_link(fun() -> sm_algorithm:schedule(#billing{slots = NewSlots, cap = Cap}, Appliances) end),
            register(algorithm, Pid),
            erlang:export(algorithm),
            sm(State#state{slots = NewSlots});
        {result, Schedule} ->
            sm_algorithm:notify(Schedule),
            sm(#state{company = Company, appliances = Schedule, sensors = Sensors});
        {_RSSI, {{NodeId, NodeIP} = Source, Content}} ->
            {NewCompany, NewAppliances, NewSensors, NewSlots} = case erlang:binary_to_term(Content) of
                company when Company == none ->
                    io:format("SM: Registered a new company ~p~n", [Source]),
                    sm_sup:add_child(sm_current, [NodeIP]),
                    {NodeIP, Appliances, Sensors, Slots};
                company when Company == NodeIP ->
                    {Company, Appliances, Sensors, Slots};
                company ->
                    io:format("SM: Already registered to the company ~p (new request from ~p)~n", [Company, NodeIP]),
                    {Company, Appliances, Sensors, Slots};
                {company, cap, NewCap} ->
                    self() !{schedule, Slots, NewCap},
                    {Company, Appliances, Sensors, Slots};
                appliance ->
                    case dict:is_key(NodeId, Appliances) of
                        true ->
                            {Company, Appliances, Sensors, Slots};
                        false ->
                            io:format("SM: Registered a new appliance ~p~n", [Source]),
                            {Company, dict:store(NodeId, #appliance{name = NodeId, ip = NodeIP}, Appliances), Sensors, Slots}
                    end;
                {appliance, data, Pid, Params} when is_pid(Pid) andalso is_list(Params) ->
                    case dict:is_key(NodeId, Appliances) of
                        true ->
                            {Company, dict:store(NodeId, #appliance{name = NodeId, ip = NodeIP, pid = Pid, params = Params}, Appliances), Sensors, Slots};
                        false ->
                            {Company, Appliances, Sensors, Slots}
                    end;
                {appliance, code, Name, Code, Hash, Filename} when is_binary(Code) ->
                    case dict:is_key(NodeId, Appliances) of
                        true ->
                            {Pid, Params} = sm_sup:start_model(Name, Code, Hash, Filename),
                            {Company, dict:store(NodeId, #appliance{name = NodeId, ip = NodeIP, pid = Pid, params = Params}, Appliances), Sensors, Slots};
                        false ->
                            {Company, Appliances, Sensors, Slots}
                    end;
                sensor ->
                    case lists:member(NodeIP, Sensors) of
                        true ->
                            {Company, Appliances, Sensors, Slots};
                        false ->
                            io:format("SM: Registered a new sensor ~p~n", [Source]),
                            {Company, Appliances, [NodeIP|Sensors], Slots}
                    end;
                Any ->
                    io:format("SM: Unknown device ~p~n", [Any]),
                    {Company, Appliances, Sensors, Slots}
            end,
            sm(#state{company = NewCompany, appliances = NewAppliances, sensors = NewSensors, slots = NewSlots});
        Any ->
            io:format("SM: Unknown message ~p~n", [Any]),
            sm(State)
    end.
