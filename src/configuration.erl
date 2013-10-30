-module(configuration).
-export([get_env/2, set_env/2]).
-define(APPLICATION, configuration).

% Public API

get_env(Key, Default) ->
    case application:get_env(?APPLICATION, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

set_env(Key, Value) ->
    application:set_env(?APPLICATION, Key, Value).

% Private API


