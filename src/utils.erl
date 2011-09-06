%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Utility functions.
-module(utils).
-export([update_tau/3, random/1, format/2, rpc/2, nodeaddr/1, nodeid/1, gethostip/0]).

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

%% @doc Update tau with the double of its current value. If
%% the new value is higher than the max, set it to max; if
%% it is zero, set it to min.
%% @spec update_tau(integer(), integer(), integer()) -> integer()
-spec(update_tau(OldTau::integer(), TauMin::integer(), TauMax::integer()) -> integer()).
update_tau(OldTau, TauMin, _) when OldTau == 0 ->
    TauMin;
update_tau(OldTau, _, TauMax) when OldTau*2 =< TauMax ->
    OldTau*2;
update_tau(_, _, TauMax) ->
    TauMax.

%% @doc Get a random value, uniformly distributed between
%% {tau/2, tau}.
%% @spec random(integer()) -> integer()
-spec(random(Tau::integer()) -> integer()).
random(Tau) ->
    N = random:uniform(),
    erlang:round(Tau/2 + erlang:round(N*Tau/2)).

%% @doc Insert the given elements into a string, returning it
%% as a string itself.
%% @spec format(string(), [any()]) -> string()
%% @see io:format/2
-spec(format(string(), [any()]) -> string()).
format(String, Elements) ->
    Pass = io_lib:format(String, Elements),
    lists:flatten(Pass).

%% @doc Send a message to a certain process and wait for
%% an answer.
%% @spec rpc(pid(), any()) -> any()
-spec(rpc(pid(), any()) -> any()).
rpc(Pid, Message) ->
    Pid ! {self(), Message},
    receive
        {_, Response} ->
            Response
    end.

%% @doc Converts a nodeid into its address.
%% @spec nodeaddr(atom()) -> integer()
%% @see utils:nodeid/1
-spec(nodeaddr(atom()) -> integer()).
nodeaddr(NodeId) ->
    list_to_integer(string:substr(atom_to_list(NodeId), 6)).

%% @doc Converts a node address into the corresponding nodeid.
%% @spec nodeid(integer()) -> atom()
%% @see utils:nodeaddr/1
-spec(nodeid(integer()) -> atom()).
nodeid(NodeAddr) ->
    list_to_atom("mote_" ++ NodeAddr).

% Private API
