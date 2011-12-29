%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Ping module.
-module(wsn_ping).
-export([start_link/0, ping/0]).
-include("wsn.hrl").
-define(TIMEOUT, 60000).
-define(PORT, 40001).

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, ping, []),
    {ok, Pid}.

ping() ->
    case gen_udp:open(?PORT, [{broadcast, true}]) of
        {ok, Socket} ->
            gen_udp:controlling_process(Socket, self()),
            erlang:send_after(?TIMEOUT, self(), wakeup),
            ping(Socket);
        {error, Reason} ->
            io:format("Unable to open UDP socket: ~p~n", [Reason])
    end.

% Private API

ping(Socket) ->
    receive
        wakeup ->
            case gen_udp:send(Socket, utils:get_bcast_addr(), ?PORT, "") of
                ok ->
                    io:format("UDP sender has sent~n"),
                    ok;
                {error, Reason} ->
                    io:format("UDP sender error ~p~n", [Reason])
            end,
            erlang:send_after(?TIMEOUT, self(), wakeup),
            ping(Socket);
        {udp, _Socket, IP, _InPortNo, _Packet} ->
            io:format("UDP packet received from ~p~n", [IP]),
            net_adm:ping(erlang:list_to_atom(?NODENAME ++ "@" ++ inet_parse:ntoa(IP))),
            ping(Socket);
        Any ->
            io:format("UDP server receiving ~p~n", [Any]),
            ping(Socket)
    end.
