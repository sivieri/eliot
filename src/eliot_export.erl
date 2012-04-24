%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc eliot export module.
-module(eliot_export).
-behaviour(gen_server).
-export([start_link/0, export_real/1, export_simulated/1, unexport/1, is_exported/1, get_exported/0, get_exported/1, get_exported_real/0, get_exported_real/1, get_exported_simulated/0, get_exported_simulated/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {real, simulated}).

% Public API

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

export_real(Subject) ->
	gen_server:cast(eliot_export, {export, real, Subject}).

export_simulated(Subject) ->
    gen_server:cast(eliot_export, {export, simulated, Subject}).

unexport(Subject) ->
	gen_server:cast(eliot_export, {unexport, Subject}).

is_exported(Subject) ->
	gen_server:call(eliot_export, {is_exported, Subject}).

get_exported() ->
	gen_server:call(eliot_export, {get}).

get_exported(StartName) ->
    gen_server:call(eliot_export, {get, StartName}).

get_exported_real() ->
    gen_server:call(eliot_export, {get, real}).

get_exported_simulated() ->
    gen_server:call(eliot_export, {get, simulated}).

get_exported_real(StartName) ->
    gen_server:call(eliot_export, {get, real, StartName}).

get_exported_simulated(StartName) ->
    gen_server:call(eliot_export, {get, simulated, StartName}).

init([]) ->
    {ok, #state{real = [], simulated = []}}.

handle_call({is_exported, Subject}, _From, State = #state{real = Real, simulated = Simulated}) ->
    Reply = lists:member(Subject, Real ++ Simulated),
    {reply, Reply, State};
handle_call({get}, _From, State = #state{real = Real, simulated = Simulated}) ->
    {reply, Real ++ Simulated, State};
handle_call({get, real}, _From, State = #state{real = Real, simulated = _Simulated}) ->
    {reply, Real, State};
handle_call({get, simulated}, _From, State = #state{real = _Real, simulated = Simulated}) ->
    {reply, Simulated, State};
handle_call({get, StartName}, _From, State = #state{real = Real, simulated = Simulated}) ->
    AtomList = lists:filter(fun(Elem) when is_atom(Elem) -> true;
                               (_Elem) -> false end, Real ++ Simulated),
    ResultList = lists:filter(fun(Name) ->
                                      StrName = atom_to_list(Name),
                                      StrStartName = atom_to_list(StartName),
                                      case string:str(StrName, StrStartName) of
                                          1 ->
                                              true;
                                          _Any ->
                                              false
                                      end end, AtomList),
    {reply, ResultList, State};
handle_call({get, real, StartName}, _From, State = #state{real = Real, simulated = _Simulated}) ->
    AtomList = lists:filter(fun(Elem) when is_atom(Elem) -> true;
                               (_Elem) -> false end, Real),
    ResultList = lists:filter(fun(Name) ->
                                      StrName = atom_to_list(Name),
                                      StrStartName = atom_to_list(StartName),
                                      case string:str(StrName, StrStartName) of
                                          1 ->
                                              true;
                                          _Any ->
                                              false
                                      end end, AtomList),
    {reply, ResultList, State};
handle_call({get, simulated, StartName}, _From, State = #state{real = _Real, simulated = Simulated}) ->
    AtomList = lists:filter(fun(Elem) when is_atom(Elem) -> true;
                               (_Elem) -> false end, Simulated),
    ResultList = lists:filter(fun(Name) ->
                                      StrName = atom_to_list(Name),
                                      StrStartName = atom_to_list(StartName),
                                      case string:str(StrName, StrStartName) of
                                          1 ->
                                              true;
                                          _Any ->
                                              false
                                      end end, AtomList),
    {reply, ResultList, State}.

handle_cast({export, real, Subject}, _State = #state{real = Real, simulated = Simulated}) ->
    {noreply, #state{real = [Subject|Real], simulated = Simulated}};
handle_cast({export, simulated, Subject}, _State = #state{real = Real, simulated = Simulated}) ->
    {noreply, #state{real = Real, simulated = [Subject|Simulated]}};
handle_cast({unexport, Subject}, _State = #state{real = Real, simulated = Simulated}) ->
	NewReal = lists:delete(Subject, Real),
    NewSimulated = lists:delete(Subject, Simulated),
    {noreply, #state{real = NewReal, simulated = NewSimulated}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API
