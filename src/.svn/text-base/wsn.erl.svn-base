%erl -sname master -rsh ssh -connect_all false
%slave:start('lap-cugola',nodo1).
%net_adm:ping('paolo@mini-cugola').
%process_info(self(), registered_name).
%java net.tinyos.sim.LinkLayerModel topologyConfig.txt
% dB = 10*Log_10(P_received/P_sent)
% P_received = 10^(gain/10) P_sent

%% @author Gianpaolo Cugola <cugola@elet.polimi.it>
%% @doc Main Wireless Sensors Network simulator. 
-module(wsn).
-export([read_net/1, spawn_net/3, send/2, send/3, execute/4, forwarder/1, echo/0]).
-define(NOISE_AVG, -75.0).
-define(NOISE_DELTA, 5.0).
-define(SENSITIVITY, 4.0).
-type(net()::{[atom()], dict()}).

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
    register(forwarder, spawn(?MODULE, forwarder, [Net])),
    {NodeIds,_} = Net,
    lists:foreach(fun(N) -> register(N, spawn(?MODULE, execute, [Module, Function, N, utils:nodeaddr(N)])) end, NodeIds).

%% @spec execute(atom(), atom(), atom(), integer()) -> any()
-spec(execute(atom(), atom(), atom(), integer()) -> any()).
execute(Module, Function, NodeId, NodeAddr) -> 
    put(myid, NodeId),
    put(myaddr, NodeAddr),
    Module:Function(). 

%% @doc Sends a message to the neighboring nodes.
%% @spec send(atom(), any()) -> ok
-spec(send(atom(), any()) -> ok).
send(SourceId, Msg) ->
    forwarder ! {SourceId, Msg},
	ok.

%% @doc Unicast, necessary for CTP.
%% @spec send(atom(), atom(), any()) -> ok
-spec(send(atom(), atom(), any()) -> ok).
send(SourceId, DestId, Msg) ->
    forwarder ! {SourceId, DestId, Msg},
	ok.

%% @spec forwarder(net()) -> ok
-spec(forwarder(net()) -> ok).
forwarder(Net) ->
    receive
	    {SourceId, Msg} ->
	        send_to_all(SourceId, Msg, element(1,Net), element(2,Net)),
	        forwarder(Net);
        {SourceId, DestId, Msg} ->
            send_to_one(SourceId, Msg, DestId, element(2, Net)),
            forwarder(Net);
        X ->
	       io:format("forwarder received ~p~n", [X])
    end.

%% @spec echo() -> none()
-spec(echo() -> none()).
echo() ->
    receive
	resend ->
	    send(get(myid), "AAAA"),
	    echo();
	{SourceId, RSSI, Msg} ->
	    io:format("Node ~p received: ~p from ~p with RSSI=~p~n", [get(myid), Msg, SourceId, RSSI]),
	    echo()
    end.

% Private API

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
            DestId ! {SourceId, RSSI, Msg};
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
    NoiseDb = ?NOISE_AVG+random:uniform()*?NOISE_DELTA*2-?NOISE_DELTA,
    SignalDb = dict:fetch({SourceId, DestId}, Gains),
    % sig = 10^(sig_dB/10)
    % noise = 10^(noise_dB/10)
    % sig/noise = 10^(sig_dB-noise_dB)/10 --in dB--> (sig_dB-noise_dB) -->
    % sig/noise > sensib (=4dB) --in dB--> (sig_dB-noise_dB) > 4
    if
	(SignalDb - NoiseDb) > ?SENSITIVITY ->
	    % RSSI = Signal+Noise
	    RSSI = round(10.0 * math:log10(math:pow(10.0, SignalDb/10.0) + math:pow(10.0, NoiseDb/10.0))),
	    DestId ! {SourceId, RSSI, Msg};
	true ->
	    ok
    end,
    send_to_all(SourceId, Msg, Nodes, Gains).
