-module(udp_dist).
-export([childspecs/0, listen/1, accept/1, accept_connection/5,
         setup/5, close/1, select/1, is_node_name/1]).
-export([accept_loop/2, do_accept/6, do_setup/6, getstat/1,tick/1]).
-export([split_node/3, splitnode/2, beacon/0]).
-import(error_logger,[error_msg/2]).
-include("net_address.hrl").
-include("dist.hrl").
-include("dist_util.hrl").
-include("eliot.hrl").
-define(to_port(Socket, Data),
        case udp:send(Socket, Data) of
            {error, closed} ->
                self() ! {udp_closed, Socket},
                {error, closed};
            R ->
                R
        end).
-define(BEACON_TIMEOUT, 10).

% Public API

%% -------------------------------------------------------------
%% This function should return a valid childspec, so that 
%% the primitive ssl_server gets supervised
%% -------------------------------------------------------------
childspecs() ->
    {ok, [{udp_server,{udp_server, start_link, []},
           permanent, 2000, worker, [udp_server]}]}.

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

select(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
    [_,_Host] -> 
        true;
    _ -> 
        false
    end.

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

listen(Name) ->
    case udp:listen(atom_to_list(Name)) of
        {ok, Socket} ->
            % If this node is accessible, then start receiving beacons
            ?trace("DEBUG: Ready to receive beacons...",[]),
            spawn_link(fun() -> beacon() end),
            {ok, {Socket, 
                  #net_address{address = [], 
                               host = inet:gethostname(),
                               protocol = udp, 
                               family = udp}, 
                  udp:get_creation(Socket)}};
        Error ->
            Error
    end.

%% ------------------------------------------------------------
%% Create the beacon part, so that when an answer is received, a new
%% connection attempt is made.
%% ------------------------------------------------------------

beacon() ->
    Pid = spawn_link(fun() -> do_clean(udp:clean_create()) end),
    erlang:send_after(?BEACON_TIMEOUT * 1000, Pid, clean),
    do_beacon(udp:beacon_create()).

do_beacon(Port) ->
    case udp:beacon(Port) of
        {ok, Host} ->
            IP = int_to_ip(Host),
            Name = erlang:list_to_atom(?NODENAME ++ "@" ++ inet_parse:ntoa(IP)),
            case node() of
                Name ->
                    ok;
                _ ->
                    ?trace("DEBUG: New beacon received from ~p~n", [Name]),
                    spawn(fun() -> net_kernel:connect_node(Name) end)
            end;
        Error ->
            Error
    end,
    do_beacon(Port).

do_clean(Port) ->
    receive
        clean ->
            ?trace("DEBUG: Time to cleanup!~n", []),
            case udp:clean(Port) of
                {ok, []} ->
                    ok;
                {ok, Hosts} ->
                    lists:foreach(fun(Host) -> 
                                          ?trace("DEBUG: Disconnecting from ~p~n", [Host]),
                                          net_kernel:disconnect(Host) end, parse_ips(Hosts));
                Error ->
                    Error
            end
    end,
    erlang:send_after(?BEACON_TIMEOUT * 1000, self(), clean),
    do_clean(Port).

%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

accept(Listen) ->
    spawn_link(?MODULE, accept_loop, [self(), Listen]).

accept_loop(Kernel, Listen) ->
    process_flag(priority, max),
    case udp:accept(Listen) of
        {ok, Socket} ->
            Kernel ! {accept,self(),Socket,udp,udp},
            controller(Kernel, Socket),
            accept_loop(Kernel, Listen);
        Error ->
            exit(Error)
    end.

controller(Kernel, Socket) ->
    receive
        {Kernel, controller, Pid} ->
            udp:controlling_process(Socket, Pid),
            Pid ! {self(), controller};
        {Kernel, unsupported_protocol} ->
            exit(unsupported_protocol)
    end.

%% ------------------------------------------------------------
%% Accepts a new connection attempt from another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

accept_connection(AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    ?trace("DEBUG: Accepting a new connection...~n", []),
    spawn_link(?MODULE, do_accept,
               [self(), AcceptPid, Socket, MyNode,
                Allowed, SetupTime]).

do_accept(Kernel, AcceptPid, Socket, MyNode, Allowed, SetupTime) ->
    process_flag(priority, max),
    receive
        {AcceptPid, controller} ->
            ?trace("DEBUG: Starting handshake (receiver side)~n", []),
            Timer = dist_util:start_timer(SetupTime),
            HSData = #hs_data{
                              kernel_pid = Kernel,
                              this_node = MyNode,
                              socket = Socket,
                              timer = Timer,
                              this_flags = ?DFLAG_PUBLISHED bor
                                               ?DFLAG_ATOM_CACHE bor
                                               ?DFLAG_EXTENDED_REFERENCES bor
                                               ?DFLAG_DIST_MONITOR bor
                                               ?DFLAG_FUN_TAGS,
                              allowed = Allowed,
                              f_send = fun(S,D) -> udp:send(S,D) end,
                              f_recv = fun(S,_N,_T) -> udp:recv(S) 
                                       end,
                              f_setopts_pre_nodeup = 
                                  fun(_S) ->
                                          ok
                                  end,
                              f_setopts_post_nodeup = 
                                  fun(S) ->
                                          udp:set_mode(S, data)
                                  end,
                              f_getll = fun(S) ->
                                                udp:get_port(S)
                                        end,
                              f_address = fun get_remote_id/2,
                              mf_tick = {?MODULE, tick},
                              mf_getstat = {?MODULE,getstat}
                             },
            dist_util:handshake_other_started(HSData),
            ?trace("DEBUG: Handshake done~n", [])
    end.

%% ------------------------------------------------------------
%% Get remote information about a Socket.
%% ------------------------------------------------------------

get_remote_id(_Socket, Node) ->
    [_, Host] = split_node(atom_to_list(Node), $@, []),
    #net_address {
                  address = [],
                  host = Host,
                  protocol = udp,
                  family = udp }.

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

setup(Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    spawn_link(?MODULE, do_setup, [self(),
                                   Node,
                                   Type,
                                   MyNode,
                                   LongOrShortNames,
                                   SetupTime]).

do_setup(Kernel, Node, Type, MyNode, LongOrShortNames,SetupTime) ->
    process_flag(priority, max),
    ?trace("~p~n",[{udp_dist,self(),setup,Node}]),
    [Name, Address] = splitnode(Node, LongOrShortNames),
     case inet:getaddr(Address, inet) of
         {ok, IP} ->
            Timer = dist_util:start_timer(SetupTime),
            case udp:connect(IP) of
                {ok, Socket} ->
                    ?trace("DEBUG: Starting handshake (sender side)~n", []),
                    HSData = #hs_data{
                                      kernel_pid = Kernel,
                                      other_node = Node,
                                      this_node = MyNode,
                                      socket = Socket,
                                      timer = Timer,
                                      this_flags = ?DFLAG_PUBLISHED bor
                                                       ?DFLAG_ATOM_CACHE bor
                                                       ?DFLAG_EXTENDED_REFERENCES bor
                                                       ?DFLAG_DIST_MONITOR bor
                                                       ?DFLAG_FUN_TAGS,
                                      other_version = 1,
                                      f_send = fun(S,D) -> 
                                                       udp:send(S,D) 
                                               end,
                                      f_recv = fun(S,_N,_T) -> 
                                                       udp:recv(S) 
                                               end,
                                      f_setopts_pre_nodeup = 
                                          fun(_S) ->
                                                  ok
                                          end,
                                      f_setopts_post_nodeup = 
                                          fun(S) ->
                                                  udp:set_mode(S, data)
                                          end,
                                      f_getll = fun(S) ->
                                                        udp:get_port(S)
                                                end,
                                      f_address = 
                                          fun(_,_) ->
                                                  #net_address{
                                                               address = {IP, 4369},
                                                               host = Address,
                                                               protocol = udp,
                                                               family = udp}
                                          end,
                                      request_type = Type,
                                      mf_tick = {?MODULE, tick},
                                      mf_getstat = {?MODULE,getstat}
                                     },
                    dist_util:handshake_we_started(HSData),
                    ?trace("DEBUG: Handshake done~n", []);
                Other ->
                    ?trace("Inner: ~p~n", [Other]),
                    ?shutdown(Node)
            end;
        _ ->
            ?trace("Outer: getaddr failed~n", []),
            ?shutdown(Node)
    end.

%%
%% Close a socket.
%%
close(Socket) ->
    udp:close(Socket).


%% If Node is illegal terminate the connection setup!!
splitnode(Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
        [Name|Tail] when Tail /= [] ->
            Host = lists:append(Tail),
            case split_node(Host, $., []) of
                [_] when LongOrShortNames == longnames ->
                    error_msg("** System running to use "
                                  "fully qualified "
                                      "hostnames **~n"
                                          "** Hostname ~s is illegal **~n",
                                          [Host]),
                    ?shutdown(Node);
                L when length(L) > 1, LongOrShortNames == shortnames ->
                    error_msg("** System NOT running to use fully qualified "
                                  "hostnames **~n"
                                      "** Hostname ~s is illegal **~n",
                                      [Host]),
                    ?shutdown(Node);
                _ ->
                    [Name, Host]
            end;
        [_] ->
            error_msg("** Nodename ~p illegal, no '@' character **~n",
                      [Node]),
            ?shutdown(Node);
        _ ->
            error_msg("** Nodename ~p illegal **~n", [Node]),
            ?shutdown(Node)
    end.

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

is_node_name(Node) when is_atom(Node) ->
    select(Node);
is_node_name(_) ->
    false.

tick(Sock) ->
    udp:tick(Sock).
getstat(Socket) ->
    udp:get_status_counters(Socket).

parse_ips(Hosts) ->
    parse_ips(Hosts, []).

parse_ips([], AccIn) ->
    AccIn;
parse_ips([A, B, C, D|T], AccIn) ->
    parse_ips(T, [erlang:list_to_atom(?NODENAME ++ "@" ++ inet_parse:ntoa(int_to_ip([A, B, C, D])))|AccIn]).

int_to_ip(<<A:8, B:8, C:8, D:8>>) ->
    {A, B, C, D};
int_to_ip([A, B, C, D]) ->
    {A, B, C, D}.
