%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc WSN export module.
-module(wsn_export).
-behaviour(gen_server).
-export([export/1, unexport/1, is_exported/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {exported}).

% Public API

export(Subject) ->
	gen_server:cast(wsn_export, {export, Subject}).

unexport(Subject) ->
	gen_server:cast(wsn_export, {unexport, Subject}).

is_exported(Subject) ->
	gen_server:call(wsn_export, {is_exported, Subject}).

init([]) ->
    {ok, #state{exported = []}}.

handle_call({is_exported, Subject}, _From, State = #state{exported = Exported}) ->
    Reply = lists:member(Subject, Exported),
    {reply, Reply, State}.

handle_cast({export, Subject}, _State = #state{exported = Exported}) ->
    {noreply, #state{exported = [Subject|Exported]}};
handle_cast({unexport, Subject}, _State = #state{exported = Exported}) ->
	NewList = lists:delete(Subject, Exported),
    {noreply, #state{exported = NewList}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API
