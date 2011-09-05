%erl -sname master -rsh ssh -connect_all false
%slave:start('lap-cugola',nodo1).
%net_adm:ping('paolo@mini-cugola').
%process_info(self(), registered_name).
%java net.tinyos.sim.LinkLayerModel topologyConfig.txt
% dB = 10*Log_10(P_received/P_sent)
% P_received = 10^(gain/10) P_sent

%% @author Gianpaolo Cugola <cugola@elet.polimi.it>
%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Main Wireless Sensors Network framework and simulator. 
-module(wsn).
-export([read_net/1, spawn_net/3, spawn_net/4, send/3, send_ignore_gain/3, execute/4, forwarder/1, echo/0, send_msg/2, spawn/3, spawn/5]).
-define(NOISE_AVG, -75.0).
-define(NOISE_DELTA, 5.0).
-define(SENSITIVITY, 4.0).
-define(OPTS, " -rsh ssh -setcookie ").
-type(net()::{[atom()], dict()} | {[{atom(), atom()}], dict()}).

% Public API

%% @doc Returns a "net", i.e., a tuple {NodeIds, Gains}, from a file of link
%% gains. NodeIds is a list of atoms [node_0, ..., node_n], and Gains is a
%% dictionary whose keys are couple {node_i, node_j} (two node ids) and values
%% are the corrensponding gains as floats. A file with link gains from a given
%% node topology can be generated using the net.tinyos.sim.LinkLayerModel Java
%% class (from tinyos) using a description of the topology.
%% @spec read_net(string()) -> net()
-spec(read_net(string()) -> net()).
read_net(Filename) ->
    {ok, Device} = file:open(Filename,[read]),
    read_net(Device, sets:new(), dict:new()).

%% @doc Spawns a net creating a process for each node. Such process executes the
%% given function and is registered under the corresponding nodeid name. The
%% dictionary is filled with two keys: myid and myaddr with the id (an atom)
%% and address (an integer) of the node.
%% @spec spawn_net(net(), atom(), atom()) -> ok
-spec(spawn_net(net(), atom(), atom()) -> ok).
spawn_net(Net, Module, Function) ->
    register(forwarder, erlang:spawn(?MODULE, forwarder, [Net])),
    {NodeIds,_} = Net,
    lists:foreach(fun(N) -> register(N, erlang:spawn(?MODULE, execute, [Module, Function, N, utils:nodeaddr(N)])) end, NodeIds).

%% @doc Spawns a net creating a process for each node, on different Erlang nodes in the
%% specified hosts: the first n-1 hosts will receive a single process, while
%% the last one will receive the remaining nodes. Such processes executes the
%% given function and is registered under the corresponding nodeid name. The
%% dictionary is filled with two keys: myid and myaddr with the id (an atom)
%% and address (an integer) of the node.
%% The forwarder node is spawned on the same host of the master node.
%% @spec spawn_net(net(), [atom()], atom(), atom()) -> ok
-spec(spawn_net(net(), [atom()], atom(), atom()) -> ok).
spawn_net(Net, Hosts, Module, Function) when length(element(1, Net)) >= length(Hosts) ->
    {ok, ForwarderNode} = slave:start_link(utils:gethostip(), forwarder,  ?OPTS ++ atom_to_list(erlang:get_cookie())),
    global:register_name(forwarder, erlang:spawn(ForwarderNode, ?MODULE, forwarder, [Net])),
    {NodeIds,_} = Net,
    {ExtNodes, LastNodes} = lists:split(length(Hosts) - 1, NodeIds),
    {ExtHosts, [Host]} = lists:split(length(Hosts) - 1, Hosts),
    ExtNodesList = lists:zip(ExtHosts, ExtNodes),
    lists:foreach(fun({ExtHost, ExtNode}) ->
                          {ok, CurNode} = slave:start_link(ExtHost, ExtNode, ?OPTS ++ atom_to_list(erlang:get_cookie())),
                          global:register_name(ExtNode, erlang:spawn(CurNode, ?MODULE, execute, [Module, Function, ExtNode, utils:nodeaddr(ExtNode)]))
                          end, ExtNodesList),
    lists:foreach(fun(N) ->
                          {ok, CurNode} = slave:start_link(Host, N, ?OPTS ++ atom_to_list(erlang:get_cookie())),
                          global:register_name(N, erlang:spawn(CurNode, ?MODULE, execute, [Module, Function, N, utils:nodeaddr(N)]))
                  end, LastNodes),
    ok;
spawn_net(Net, _Host, Module, Function) ->
    io:format("More hosts than nodes: reverting to local.~n"),
    spawn_net(Net, Module, Function).

%% @doc Send a message.
%% @spec send(atom(), atom(), any()) -> ok
-spec(send(atom(), atom(), any()) -> ok).
send(SourceId, DestId, Msg) ->
    send_msg(forwarder, {gain, SourceId, DestId, Msg}).

%% @doc Send a message ignoring gain informations.
%% @spec send_ignore_gain(atom(), atom(), any()) -> ok
-spec(send_ignore_gain(atom(), atom(), any()) -> ok).
send_ignore_gain(SourceId, DestId, Msg) ->
    send_msg(forwarder, {nogain, SourceId, DestId, Msg}).

%% @doc Spawn a process executing a fun on a remote mote.
%% This function is asynchronous, and the process invoking
%% it should expect a message in the form {spawned, pid()} | {spawned, error} | [{spawned, pid() | error}].
%% @spec spawn(atom(), atom(), function()) -> ok
-spec(spawn(atom(), atom(), function()) -> ok).
spawn(SourceId, DestId, Fun) ->
    send_msg(forwarder, {spawn, SourceId, DestId, Fun}).

%% @doc Spawn a process executing the given function on a remote mote.
%% This function is asynchronous, and the process invoking
%% it should expect a message in the form {spawned, pid()} | {spawned, error} | [{spawned, pid() | error}].
%% @spec spawn(atom(), atom(), atom(), atom(), [any()]) -> ok
-spec(spawn(atom(), atom(), atom(), atom(), [any()]) -> ok).
spawn(SourceId, DestId, Module, Function, Args) ->
    send_msg(forwarder, {spawn, SourceId, DestId, Module, Function, Args}).

%% @private
%% @spec execute(atom(), atom(), atom(), integer()) -> any()
-spec(execute(atom(), atom(), atom(), integer()) -> any()).
execute(Module, Function, NodeId, NodeAddr) ->
    put(myid, NodeId),
    put(myaddr, NodeAddr),
    Module:Function(). 

%% @private
%% @spec forwarder(net()) -> ok
-spec(forwarder(net()) -> ok).
forwarder(Net) ->
    receive
        {gain, SourceId, DestId, Msg} when DestId == all ->
            send_to_all(SourceId, Msg, element(1,Net), element(2,Net)),
            forwarder(Net);
        {gain, SourceId, DestId, Msg} ->
            send_to_one(SourceId, Msg, DestId, element(2, Net)),
            forwarder(Net);
        {nogain, SourceId, DestId, Msg} when DestId == all ->
            send_to_all_no_gain(SourceId, DestId, Msg),
            forwarder(Net);
        {nogain, _SourceId, DestId, Msg} ->
            send_to_one_no_gain(DestId, Msg),
            forwarder(Net);
        {spawn, SourceId, DestId, Fun} when DestId == all ->
            Res = lists:map(fun(E) -> spawn_remote(E, Fun) end, element(1, Net)),
            send_msg(SourceId, {spawned, Res}),
            forwarder(Net);
        {spawn, SourceId, DestId, Fun} ->
            Res = spawn_remote(DestId, Fun),
            send_msg(SourceId, {spawned, Res}),
            forwarder(Net);
        {spawn, SourceId, DestId, Module, Function, Args} when DestId == all ->
            Res = lists:map(fun(E) -> spawn_remote(E, Module, Function, Args) end, element(1, Net)),
            send_msg(SourceId, {spawned, Res}),
            forwarder(Net);
        {spawn, SourceId, DestId, Module, Function, Args} ->
            Res = spawn_remote(DestId, Module, Function, Args),
            send_msg(SourceId, {spawned, Res}),
            forwarder(Net);
        Any ->
	       io:format("forwarder received ~p~n", [Any]),
           forwarder(Net)
    end.

%% @private
%% @spec echo() -> none()
-spec(echo() -> none()).
echo() ->
    receive
	resend ->
	    send(get(myid), all, "AAAA"),
	    echo();
	{SourceId, RSSI, Msg} ->
	    io:format("Node ~p received: ~p from ~p with RSSI=~p~n", [get(myid), Msg, SourceId, RSSI]),
	    echo()
    end.

% Private API

%% @private
-spec(spawn_remote(atom(), function()) -> pid() | error).
spawn_remote(DestId, Fun) ->
    case global:whereis_name(DestId) of
        undefined ->
            error;
        Pid ->
            Pid2 = erlang:spawn(node(Pid), Fun),
            Pid2
    end.

%% @private
-spec(spawn_remote(atom(), atom(), atom(), [any()]) -> pid() | error).
spawn_remote(DestId, Module, Function, Args) ->
    case global:whereis_name(DestId) of
        undefined ->
            error;
        Node ->
            Pid = erlang:spawn(Node, Module, Function, Args),
            Pid
    end.

%% @private
-spec(send_msg(atom(), any()) -> ok).
send_msg(Dest, Msg) ->
    case global:whereis_name(Dest) of
        undefined ->
            try
                Dest ! Msg
            catch
                _:_Reason ->
                    io:format("Node ~p found ~p down~n", [node(), Dest])
            end;
        Pid ->
            Pid ! Msg
    end,
    ok.

%% @private
-spec(read_net(pid(), set(), dict()) -> net()).
read_net(Device, Nodes, Gains) ->
    case io:get_line(Device, "") of
	"gain"++Rest ->
	    [Node1, Node2, Gain] = string:tokens(Rest," \t"),
	    {G,_}=string:to_float(Gain), % remove trailing CR and LF
	    NewNodes = sets:add_element(utils:nodeid(Node1), Nodes),
	    NewGains = dict:store({utils:nodeid(Node1), utils:nodeid(Node2)}, G, Gains),
	    read_net(Device, NewNodes, NewGains);
	_Else ->
	    file:close(Device),
	    SortedNodes = lists:sort(sets:to_list(Nodes)),
	    {SortedNodes, Gains}
    end.

%% @private
-spec(send_to_one_no_gain(atom(), any()) -> ok).
send_to_one_no_gain(DestId, Msg) ->
    send_msg(DestId, Msg).

%% @private
-spec(send_to_all_no_gain(atom(), [atom()], any()) -> ok).
send_to_all_no_gain(_, [], _) ->
    ok;
send_to_all_no_gain(SourceId, [SourceId|Nodes], Msg) ->
    send_to_all_no_gain(SourceId, Nodes, Msg);
send_to_all_no_gain(SourceId, [DestId|Nodes], Msg) ->
    send_msg(DestId, Msg),
    send_to_all_no_gain(SourceId, Nodes, Msg).

%% @private
-spec(send_to_one(atom(), any(), atom(), dict()) -> ok).
send_to_one(SourceId, Msg, DestId, Gains) ->
    NoiseDb = ?NOISE_AVG + random:uniform() * ?NOISE_DELTA * 2 - ?NOISE_DELTA,
    SignalDb = dict:fetch({SourceId, DestId}, Gains),
    % sig = 10^(sig_dB/10)
    % noise = 10^(noise_dB/10)
    % sig/noise = 10^(sig_dB-noise_dB)/10 --in dB--> (sig_dB-noise_dB) -->
    % sig/noise > sensib (=4dB) --in dB--> (sig_dB-noise_dB) > 4
    if
        (SignalDb - NoiseDb) > ?SENSITIVITY ->
            % RSSI = Signal+Noise
            RSSI = round(10.0 * math:log10(math:pow(10.0, SignalDb / 10.0) + math:pow(10.0, NoiseDb / 10.0))),
            send_msg(DestId, {SourceId, RSSI, Msg});
        true ->
            ok
    end.

%% @private
-spec(send_to_all(atom(), any(), [atom()], dict()) -> ok).
send_to_all(_, _, [], _) ->
    ok;
send_to_all(SourceId, Msg, [SourceId|Nodes], Gains) ->
    send_to_all(SourceId, Msg, Nodes, Gains);
send_to_all(SourceId, Msg, [DestId|Nodes], Gains) ->
    NoiseDb = ?NOISE_AVG + random:uniform() * ?NOISE_DELTA * 2 - ?NOISE_DELTA,
    SignalDb = dict:fetch({SourceId, DestId}, Gains),
    % sig = 10^(sig_dB/10)
    % noise = 10^(noise_dB/10)
    % sig/noise = 10^(sig_dB-noise_dB)/10 --in dB--> (sig_dB-noise_dB) -->
    % sig/noise > sensib (=4dB) --in dB--> (sig_dB-noise_dB) > 4
    if
	(SignalDb - NoiseDb) > ?SENSITIVITY ->
	    % RSSI = Signal+Noise
	    RSSI = round(10.0 * math:log10(math:pow(10.0, SignalDb/10.0) + math:pow(10.0, NoiseDb/10.0))),
	    send_msg(DestId, {SourceId, RSSI, Msg});
	true ->
	    ok
    end,
    send_to_all(SourceId, Msg, Nodes, Gains).
