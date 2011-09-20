%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Utility functions.
-module(utils).
-export([format/2, gethostip/0, consistency/3]).

% Public API

%% @doc Get the IP of the active interface (avoiding loopback).
%% @spec gethostip() -> atom()
-spec(gethostip() -> atom()).
gethostip() ->
    {ok, Ifs} = inet:getiflist(),
    Ifs2 = lists:filter(fun(Elem) when Elem == "lo" -> false;
                           (_Elem) -> true end, Ifs),
    [Real|_] = Ifs2,
    {ok, [{addr, Addr}]} = inet:ifget(Real, [addr]),
    erlang:list_to_atom(inet_parse:ntoa(Addr)).

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

% Private API
