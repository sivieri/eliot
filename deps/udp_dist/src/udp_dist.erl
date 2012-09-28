-module(udp_dist).
-export([childspecs/0, listen/1, accept/1, accept_connection/5,
         setup/5, close/1, select/1, is_node_name/1]).
-export([accept_loop/2, do_accept/6, do_setup/6, getstat/1,tick/1]).
-export([split_node/3, splitnode/2]).
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

select(all) ->
    true;
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
            % Create the 'all' port
            spawn(fun() -> net_kernel:connect_node(all) end),
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
    udp:recv(Socket),
    {ok, IP} = udp:ip(Socket),
    {D, C, B, A} = int_to_ip(IP),
    ?trace("DEBUG: IP is ~p~n", [inet_parse:ntoa({A, B, C, D})]),
    OtherNode = list_to_atom(?NODENAME ++ "@" ++ inet_parse:ntoa({A, B, C, D})),
    receive
        {AcceptPid, controller} ->
            ?trace("DEBUG: Starting handshake (receiver side)~n", []),
            Timer = dist_util:start_timer(SetupTime),
            HSData = #hs_data{
                              kernel_pid = Kernel,
                              this_node = MyNode,
                              other_node = OtherNode,
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

do_setup(Kernel, all, Type, MyNode, LongOrShortNames, SetupTime) ->
    Timer = dist_util:start_timer(SetupTime),
    case udp:broadcast(inet_parse:ntoa(get_bcast_addr())) of
        {ok, Socket} ->
            ?trace("DEBUG: Starting handshake (sender side)~n", []),
            HSData = #hs_data{
                              kernel_pid = Kernel,
                              other_node = all,
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
                                                       address = {get_bcast_addr(), 4369},
                                                       host = all,
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
            ?shutdown(all)
    end;
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

int_to_ip(Ip) ->
    {Ip bsr 24, (Ip band 16711680) bsr 16, (Ip band 65280) bsr 8, Ip band 255}.
