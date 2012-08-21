-module(eliot_oppflooder_event).
-export([start_link/0, add_handler/2, delete_handler/2, notify/2]).

% Public API

start_link() ->
    gen_event:start_link({local, ?MODULE}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

notify(Source, Payload) ->
    gen_event:notify(?MODULE, {msg, Source, Payload}).

% Private APi
