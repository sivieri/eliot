-module(openhouse).
-export([start/0]).

start() ->
    application:start(sasl),
    application:start(crypto),
    application:start(crest).
