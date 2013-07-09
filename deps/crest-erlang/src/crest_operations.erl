%% Copyright (c) 2010,2011 Alessandro Sivieri <sivieri@elet.polimi.it>
%% 
%% This file is part of CREST-Erlang.
%% 
%% CREST-Erlang is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as published by
%% the Free Software Foundation, either version 3 of the License, or
%% (at your option) any later version.
%% 
%% CREST-Erlang is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Lesser General Public License for more details.
%% 
%% You should have received a copy of the GNU Lesser General Public License
%% along with CREST-Erlang. If not, see <http://www.gnu.org/licenses/>.
%% 
%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Main Computational REST operations.
%% @copyright 2010,2011 Alessandro Sivieri

-module(crest_operations).
-export([install_local/1, invoke_local_spawn/1, invoke_spawn/3, invoke_spawn/4, invoke_remote/4, invoke_remote/5, invoke_lambda/4, invoke_lambda/5, invoke_local_lambda/2]).

%% External API

%% @doc Wrapper around crest_local:start_local/1.
%% @spec install_local(string()) -> {ok, string()} | {error}
install_local(Name) ->
	crest_local:reload(),
	crest_local:start_local(Name).

%% @doc Spawn a function on this host; it can be done also through invoke_spawn,
%% but this call bypasses a Web request to localhost.
%% @spec invoke_local_spawn(fun()) -> {ok, string()}
invoke_local_spawn(Function) ->
    Key = crest_peer:spawn_local_install(Function),
    {ok, Key}.

%% @doc Spawn a function on a certain host; the function module needs to be
%% in the Erlang path.
%% @spec invoke_spawn(string(), atom(), fun()) -> {ok, Key} | {error}
invoke_spawn(Host, Module, Function) ->
    ibrowse:start(),
    Res = ibrowse:send_req("http://" ++ Host ++ ":8080/crest/spawn", [{"Content-Type", "application/x-www-form-urlencoded"}], post, crest_utils:get_lambda_params(Module, Function, [])),

    case Res of
        {ok, "200", _, Obj} ->
            Key = crest_json:destructure("Obj.key", mochijson2:decode(Obj)),
            {ok, Key};
        {ok, _, _, _} ->
            {error};
        {error, _Reason} ->
            {error}
    end.

%% @doc Spawn a function on a certain host; the function module needs to be
%% in the Erlang path.
%% @spec invoke_spawn(string(), integer(), atom(), fun()) -> {ok, Key} | {error}
invoke_spawn(Host, Port, Module, Function) ->
    ibrowse:start(),
    Res = ibrowse:send_req("http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/crest/spawn", [{"Content-Type", "application/x-www-form-urlencoded"}], post, crest_utils:get_lambda_params(Module, Function, [])),

    case Res of
        {ok, "200", _, Obj} ->
            Key = crest_json:destructure("Obj.key", mochijson2:decode(Obj)),
            {ok, Key};
        {ok, _, _, _} ->
            {error};
        {error, _Reason} ->
            {error}
    end.

%% @doc Spawn a function on a certain host and invoke it with parameters;
%% the function module needs to be in the Erlang path.
%% @spec invoke_remote(string(), atom(), fun(), [{string(), string()}]) -> {ok, Body} | {error}
invoke_remote(Host, Module, Function, Params) ->
	ibrowse:start(),
	Res = ibrowse:send_req("http://" ++ Host ++ ":8080/crest/remote", [{"Content-Type", "application/x-www-form-urlencoded"}], post, crest_utils:get_lambda_params(Module, Function, Params)),

	io:format("~p~n", [Res]),
    case Res of
        {ok, "200", _, Body} ->
            {ok, Body};
		{ok, _, _, _} ->
            {error};
		{error, _Reason} ->
            {error}
    end.

%% @doc Spawn a function on a certain host and invoke it with parameters;
%% the function module needs to be in the Erlang path.
%% @spec invoke_remote(string(), integer(), atom(), fun(), [{string(), string()}]) -> {ok, Body} | {error}
invoke_remote(Host, Port, Module, Function, Params) ->
	ibrowse:start(),
	Res = ibrowse:send_req("http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ "/crest/remote", [{"Content-Type", "application/x-www-form-urlencoded"}], post, crest_utils:get_lambda_params(Module, Function, Params)),

	io:format("~p~n", [Res]),
    case Res of
        {ok, "200", _, Body} ->
            {ok, Body};
		{ok, _, _, _} ->
            {error};
		{error, _Reason} ->
            {error}
    end.

%% @doc Invoke an already installed computation with parameters.
%% @spec invoke_lambda(get | post, string(), string(), [{string(), string()}]) -> {ok, Body} | {error}
invoke_lambda(Method, Host, Key, Params) ->
	ibrowse:start(),
	case Method of
		get ->
			Res = ibrowse:send_req("http://"++ Host ++ ":8080/crest/url/" ++ Key ++ "?" ++ mochiweb_util:urlencode(Params), [], get);
		post ->
			Res = ibrowse:send_req("http://"++ Host ++ ":8080/crest/url/" ++ Key, [{"Content-Type", "application/x-www-form-urlencoded"}], post, mochiweb_util:urlencode(Params));
		_ ->
			Res = {error, "Wrong method"}
	end,
    case Res of
        {ok, "200", _, Body} ->
            {ok, Body};
		{ok, _, _, _} ->
            {error};
		{error, _Reason} ->
            {error}
    end.

%% @doc Invoke an already installed computation with parameters.
%% @spec invoke_lambda(get | post, string(), integer(), string(), [{string(), string()}]) -> {ok, Body} | {error}
invoke_lambda(Method, Host, Port, Key, Params) ->
	ibrowse:start(),
	case Method of
		get ->
			Res = ibrowse:send_req("http://"++ Host ++ ":" ++ integer_to_list(Port) ++ "/crest/url/" ++ Key ++ "?" ++ mochiweb_util:urlencode(Params), [], get);
		post ->
			Res = ibrowse:send_req("http://"++ Host ++ ":" ++ integer_to_list(Port) ++ "/crest/url/" ++ Key, [{"Content-Type", "application/x-www-form-urlencoded"}], post, mochiweb_util:urlencode(Params));
		_ ->
			Res = {error, "Wrong method"}
	end,
    case Res of
        {ok, "200", _, Body} ->
            {ok, Body};
		{ok, _, _, _} ->
            {error};
		{error, _Reason} ->
            {error}
    end.

%% @doc Invoke an already installed computation in the same peer.
%% @spec invoke_local_lambda(string(), [{string(), string()}]) -> {ok, Body} | {error}
invoke_local_lambda(Key, Params) when length(Key) == 1 ->
    crest_peer:spawn_exec(Key, Params);
invoke_local_lambda(Key, Params) ->
    crest_peer:spawn_exec([Key], Params).

%% Internal API

