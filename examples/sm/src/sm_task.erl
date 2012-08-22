-module(sm_task).
-export([start_link/0, sm/0]).
-include("scenario.hrl").
-define(TIMER, 10 * 1000).
-record(state, {company = none, appliances = [], sensors = []}).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, sm, []),
    register(sm, Pid),
    erlang:export(sm),
    erlang:send_after(?TIMER, Pid, beacon),
    {ok, Pid}.

sm() ->
    sm(#state{}).

% Private API

sm(#state{company = Company, appliances = Appliances, sensors = Sensors} = State) ->
    receive
        beacon ->
            Msg = erlang:term_to_binary(sm),
            eliot_oppflooder:send(oppflooder, Msg),
            erlang:send_after(?TIMER, self(), beacon),
            sm(State);
        {_RSSI, {{_NodeId, NodeIP}, Content}} ->
            io:format("SM: Trying to register ~p as ~p~n", [NodeIP, erlang:binary_to_term(Content)]),
            {NewCompany, NewAppliances, NewSensors} = case erlang:binary_to_term(Content) of
                company when Company == none ->
                    {NodeIP, Appliances, Sensors};
                company when Company == NodeIP ->
                    {Company, Appliances, Sensors};
                company ->
                    io:format("SM: Already registered to the company ~p (new request from ~p)~n", [Company, NodeIP]),
                    {Company, Appliances, Sensors};
                appliance ->
                    case lists:member(NodeIP, Appliances) of
                        true ->
                            {Company, Appliances, Sensors};
                        false ->
                            {Company, [NodeIP|Appliances], Sensors}
                    end;
                sensor ->
                    case lists:member(NodeIP, Sensors) of
                        true ->
                            {Company, Appliances, Sensors};
                        false ->
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
