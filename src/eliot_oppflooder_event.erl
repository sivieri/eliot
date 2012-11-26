-module(eliot_oppflooder_event).
-export([start/0, start_link/0, add_handler/2, delete_handler/2, notify/2]).
-ifdef(simulation).
-define(GENEVTNAME, eliot_simulator:get_simname(?MODULE)).
-else.
-define(GENEVTNAME, ?MODULE).
-endif.

% Public API

start() ->
    gen_event:start({local, ?GENEVTNAME}).

start_link() ->
    gen_event:start_link({local, ?GENEVTNAME}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?GENEVTNAME, {Handler, eliot_api:id()}, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?GENEVTNAME, {Handler, eliot_api:id()}, Args).

notify(Source, Payload) ->
    gen_event:notify(?GENEVTNAME, {msg, Source, Payload}).

% Private APi
