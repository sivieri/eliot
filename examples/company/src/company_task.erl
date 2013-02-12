-module(company_task).
-export([start_link/0, company/0, set_cap/1]).
-include("eliot.hrl").
-include("scenario.hrl").
-record(state, {sm = none, cap = 3000, slots = [#slot{starttime = {7, 00}, endtime = {20, 00}, priority = 0},
                     #slot{starttime = {20, 00}, endtime = {7, 00}, priority = 1}]}).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, company, []),
    register(sm, Pid),
    erlang:export(sm),
    {ok, Pid}.

company() ->
    company(#state{}).

set_cap(Cap) ->
    sm ! {cap, Cap}.

% Private API

company(#state{sm = SM, cap = Cap, slots = Slots} = State) ->
    receive
        {cap, NewCap} ->
            Dest = eliot_api:ip_to_node(SM),
            {sm, Dest} ~ <<?COMPANY:8/unsigned-little-integer, Cap:16/unsigned-little-integer>>,
            company(State#state{cap = NewCap});
        {_RSSI, Source, Content} ->
            case Content of
                <<?CONSUMPTION:8/unsigned-little-integer, Value:16/unsigned-little-integer>> ->
                    io:format("Company: ~p is consuming ~p KWh~n", [Source, Value]);
                <<?SM:8/unsigned-little-integer>> ->
                    if
                        SM == none ->
                            io:format("Company: Registering to SM ~p~n", [Source]),
                            Bin1 = data:encode_slots(Slots),
                            {sm, eliot_api:ip_to_node(Source)} ~ <<?COMPANY:8/unsigned-little-integer, Cap:16/unsigned-little-integer, Bin1/binary>>,
                            company(State#state{sm = Source});
                        SM == Source ->
                            company(State);
                        true ->
                            io:format("Company: Already registered to SM ~p, changing to ~p~n", [SM, Source]),
                            Bin1 = data:encode_slots(Slots),
                            {sm, eliot_api:ip_to_node(Source)} ~ <<?COMPANY:8/unsigned-little-integer, Cap:16/unsigned-little-integer, Bin1/binary>>,
                            company(State#state{sm = Source})
                    end;
                <<?RESET:8/unsigned-little-integer>> ->
                    company(State#state{sm = none});
                Any ->
                    io:format("Company: Unknown binary message ~p~n", [Any])
            end,
            company(State);
        Any ->
            io:format("Company: Unknown message ~p~n", [Any]),
            company(State)
    end.
