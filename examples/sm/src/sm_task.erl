-module(sm_task).
-export([start_link/0, sm/0, schedule/2]).
-include("scenario.hrl").
-define(TIMER, 10 * 1000).
-record(state, {company = none, appliances = dict:new(), sensors = []}).

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

% Private API

sm(#state{company = Company, appliances = Appliances, sensors = Sensors} = State) ->
    receive
        beacon ->
            Msg = erlang:term_to_binary(sm),
            eliot_oppflooder:send(oppflooder, Msg),
            erlang:send_after(?TIMER, self(), beacon),
            sm(State);
        {schedule, Slots, Cap} ->
            Pid = spawn_link(fun() -> sm_algorithm:schedule(#billing{slots = Slots, cap = Cap}, Appliances) end),
            register(algorithm, Pid),
            erlang:export(algorithm),
            sm(State);
        {result, Schedule} ->
            sm_algorithm:notify(Schedule),
            sm(#state{company = Company, appliances = Schedule, sensors = Sensors});
        {_RSSI, {{NodeId, NodeIP} = Source, Content}} ->
            {NewCompany, NewAppliances, NewSensors} = case erlang:binary_to_term(Content) of
                company when Company == none ->
                    io:format("SM: Registered a new company ~p~n", [Source]),
                    {NodeIP, Appliances, Sensors};
                company when Company == NodeIP ->
                    {Company, Appliances, Sensors};
                company ->
                    io:format("SM: Already registered to the company ~p (new request from ~p)~n", [Company, NodeIP]),
                    {Company, Appliances, Sensors};
                appliance ->
                    case dict:is_key(NodeId, Appliances) of
                        true ->
                            {Company, Appliances, Sensors};
                        false ->
                            io:format("SM: Registered a new appliance ~p~n", [Source]),
                            {Company, dict:store(NodeId, #appliance{name = NodeId, ip = NodeIP}, Appliances), Sensors}
                    end;
                {appliance, data, Pid, Params} when is_pid(Pid) andalso is_list(Params) ->
                    case dict:is_key(NodeId, Appliances) of
                        true ->
                            {Company, dict:store(NodeId, #appliance{name = NodeId, ip = NodeIP, pid = Pid, params = Params}, Appliances), Sensors};
                        false ->
                            {Company, Appliances, Sensors}
                    end;
                sensor ->
                    case lists:member(NodeIP, Sensors) of
                        true ->
                            {Company, Appliances, Sensors};
                        false ->
                            io:format("SM: Registered a new sensor ~p~n", [Source]),
                            {Company, Appliances, [NodeIP|Sensors]}
                    end;
                Any ->
                    io:format("SM: Unknown device ~p~n", [Any]),
                    {Company, Appliances, Sensors}
            end,
            sm(#state{company = NewCompany, appliances = NewAppliances, sensors = NewSensors});
        Any ->
            io:format("SM: Unknown message ~p~n", [Any]),
            sm(State)
    end.
