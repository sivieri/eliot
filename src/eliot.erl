%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Start the application.
-module(eliot).
-export([start/0, stop/0]).

% Public API

start() ->
	application:start(eliot).

stop() ->
	application:stop(eliot).

% Private API
