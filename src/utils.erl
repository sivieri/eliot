%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Utility functions.
-module(utils).
-export([format/2, get_host_ip/0, consistency/3, echo/0, get_bcast_addr/0, to_int/1, get_host_mac/0, split_name/1, join_name/2]).
-include("eliot.hrl").

% Public API

%% @doc Insert the given elements into a string, returning it
%% as a string itself.
%% @spec format(string(), [any()]) -> string()
%% @see io:format/2
-spec(format(string(), [any()]) -> string()).
format(String, Elements) ->
    Pass = io_lib:format(String, Elements),
    lists:flatten(Pass).

%% @doc Check the consistency of the hosts list.
%% @spec consistency([{atom(), integer()}], integer(), integer()) -> boolean()
-spec(consistency([{atom(), integer()}], integer(), integer()) -> boolean()).
consistency([], Acc, N) when N == Acc ->
    true;
consistency([], _Acc, _N) ->
    false;
consistency([{_IP, I}|T], Acc, N) ->
    consistency(T, Acc + I, N).

-spec(echo() -> ok).
echo() ->
    receive
        Any ->
            io:format("~p~n", [Any]),
            echo()
    end.

get_bcast_addr() ->
    case inet:getifaddrs() of
        {ok, IfList} when length(IfList) == 2 ->
            [{_Real, IfOpts}] = lists:filter(fun({Name, _IfOpts}) when Name == "lo" -> false;
                                     ({_Name, _IfOpts}) -> true end, IfList),
            {broadaddr, Address} = lists:keyfind(broadaddr, 1, IfOpts),
            Address;
        {ok, IfList} ->
            {?INTERFACE, IfOpts} = lists:keyfind(?INTERFACE, 1, IfList),
            {broadaddr, Address} = lists:keyfind(broadaddr, 1, IfOpts),
            Address
    end.

get_host_ip() ->
    case inet:getifaddrs() of
        {ok, IfList} when length(IfList) == 2 ->
            [{_Real, IfOpts}] = lists:filter(fun({Name, _IfOpts}) when Name == "lo" -> false;
                                     ({_Name, _IfOpts}) -> true end, IfList),
            {broadaddr, Address} = lists:keyfind(broadaddr, 1, IfOpts),
            Address;
        {ok, IfList} ->
            {?INTERFACE, IfOpts} = lists:keyfind(?INTERFACE, 1, IfList),
            {addr, Address} = lists:keyfind(addr, 1, IfOpts),
            Address
    end.

get_host_mac() ->
    case inet:getifaddrs() of
        {ok, IfList} when length(IfList) == 2 ->
            [{_Real, IfOpts}] = lists:filter(fun({Name, _IfOpts}) when Name == "lo" -> false;
                                     ({_Name, _IfOpts}) -> true end, IfList),
            {broadaddr, Address} = lists:keyfind(broadaddr, 1, IfOpts),
            Address;
        {ok, IfList} ->
            {?INTERFACE, IfOpts} = lists:keyfind(?INTERFACE, 1, IfList),
            {hwaddr, Address} = lists:keyfind(hwaddr, 1, IfOpts),
            StringAddress = lists:map(fun(X) -> format("~2.16.0b", [X]) end, Address),
            string:join(StringAddress, ":")
    end.

split_name(Node) ->
    NodeString = erlang:atom_to_list(Node),
    [NodeName, NodeAddress] = string:tokens(NodeString, "@"),
    {NodeName, NodeAddress}.

join_name(NodeName, NodeAddress) ->
    erlang:list_to_atom(NodeName ++ "@" ++ NodeAddress).

to_int(String) ->
    {Res, _} = string:to_integer(String),
    Res.

% Private API
