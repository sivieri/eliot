-module(eliot_mac).
-behaviour(gen_server).
-export([start_link/0, set/2, get/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {tableref}).

% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set(Ip, Mac) ->
    gen_server:cast(?MODULE, {set, Ip, Mac}).

get(Ip) ->
    gen_server:call(?MODULE, {get, Ip}).

init([]) ->
    {ok, #state{tableref = ets:new(macs, [set])}}.

handle_call({get, Ip}, _From, #state{tableref = Ref} = State) ->
    Res = ets:lookup(Ref, Ip),
    {reply, Res, State}.

handle_cast({set, Ip, Mac}, #state{tableref = Ref} = State) ->
    ets:insert(Ref, {Ip, Mac}),
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API
