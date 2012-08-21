-module(oppflooder_handler).
-behaviour(gen_event).
-export([add_handler/0, delete_handler/0]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

% Public API

add_handler() ->
    eliot_oppflooder_event:add_handler(?MODULE, []).

delete_handler() ->
    eliot_oppflooder_event:delete_handler(?MODULE, []).

init([]) ->
    {ok, #state{}}.

handle_event({msg, Src, Payload}, State) ->
    io:format("CLIENT: Received ~p from ~p~n", [erlang:binary_to_term(Payload), Src]),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API
