%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Main Wireless Sensors Network framework and simulator.
-module(wsn).
-export([read_net/1, moteid/1, moteaddr/1]).
-export([myaddr/0, register/2, send/3, bcast/2, spawn/2, spawn/4, bcast_spawn/1, bcast_spawn/3]).
-type(net()::{[atom()], dict()} | {[{atom(), atom()}], dict()}).

% Public API

-spec(read_net(string()) -> net()).
read_net(Filename) ->
    {ok, Device} = file:open(Filename,[read]),
    read_net(Device, sets:new(), dict:new()).

-spec(moteid(integer() | string()) -> atom()).
moteid(NodeAddr) when is_integer(NodeAddr) ->
    list_to_atom("mote_" ++ utils:format("~p", [NodeAddr]));
moteid(NodeAddr) ->
    list_to_atom("mote_" ++ NodeAddr).

-spec(moteaddr(atom()) -> integer()).
moteaddr(NodeId) ->
    list_to_integer(string:substr(atom_to_list(NodeId), 6)).

myaddr() ->
	ok.

register(Name, Pid) ->
	ok.

send(Name, NodeAddr, Msg) ->
	ok.

bcast(Name, Msg) ->
	ok.

spawn(NodeAddr, Fun) ->
	ok.

spawn(NodeAddr, Module, Function, Args) ->
	ok.

bcast_spawn(Fun) ->
	ok.

bcast_spawn(Module, Function, Args) ->
	ok.

% Private API

-spec(read_net(pid(), set(), dict()) -> net()).
read_net(Device, Nodes, Gains) ->
    case io:get_line(Device, "") of
	"gain"++Rest ->
	    [Node1, Node2, Gain] = string:tokens(Rest," \t"),
	    {G,_}=string:to_float(Gain), % remove trailing CR and LF
	    NewNodes = sets:add_element(moteid(Node1), Nodes),
	    NewGains = dict:store({moteid(Node1), moteid(Node2)}, G, Gains),
	    read_net(Device, NewNodes, NewGains);
	_Else ->
	    file:close(Device),
	    SortedNodes = lists:sort(sets:to_list(Nodes)),
	    {SortedNodes, Gains}
    end.
