%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Start the application.
-module(wsn).
-export([start/0, stop/0]).

% Public API

start() ->
	application:start(wsn).

stop() ->
	application:stop(wsn).

% Private API
