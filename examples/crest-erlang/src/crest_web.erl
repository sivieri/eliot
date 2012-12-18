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
%% @doc Web server for crest.
%% Autogenerated by MochiWeb.
%% @copyright 2010,2011 Alessandro Sivieri

-module(crest_web).
-export([start/1, stop/0, loop/2]).

%% External API

%% @doc Start the Web server.
%% @spec start([any()]) -> ok
start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

%% @doc Stop the Web server.
%% @spec stop() -> ok
stop() ->
    mochiweb_http:stop(?MODULE).

%% @doc Main server loop, serving requests.
%% @spec loop(request(), string()) -> any()
loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    log4erl:info("Request: ~p~n", [Path]),
    case Req:get(method) of
        Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case string:tokens(Path, "/") of
                ["crest", "manager", "installed"] ->
                    Req:respond({200, [{"Content-Type", "application/json"}], [mochijson2:encode(crest_manager:get_installed_data())]});
				["crest", "manager", "local"] ->
                    Req:respond({200, [{"Content-Type", "application/json"}], [mochijson2:encode(crest_manager:get_local_data())]});
				["crest", "spawn"] ->
					Req:respond({404, [], []});
				["crest", "remote"] ->
					Req:respond({404, [], []});
                ["crest", "url"|T] ->
                    case crest_peer:spawn_exec(T, Req:parse_qs()) of
                        {ok, {CT, Message}} ->
                            Req:respond({200, [{"Content-Type", CT}], [Message]});
                        {ok} ->
                            Req:respond({200, [], []});
                        {error} ->
                            Req:respond({404, [], []})
                    end;
		["crest", "local", T] ->
		    case crest_local:start_local(T) of
                        {ok, Key} ->
                            Req:respond({200, [{"Content-Type", "application/json"}], mochijson2:encode(crest_utils:pack_key(Key))});
                        {error} ->
                            Req:respond({404, [], []})
                    end;
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        'POST' ->
            case string:tokens(Path, "/") of
		%%["crest", "spawn"] ->
		%%	Req:respond({404, [], []});
		%%["crest", "remote"] ->
		%%	Req:respond({404, [], []});
 		["crest", "spawn"] ->
		    Key = crest_peer:spawn_install(Req:parse_post()),
                    Req:respond({200, [{"Content-Type", "application/json"}], [mochijson2:encode(crest_utils:pack_key(Key))]});
		["crest", "remote"] ->
   		    case crest_peer:remote(Req:parse_post()) of
                        {ok, {CT, Message}} ->
                            	Req:respond({200, [{"Content-Type", CT}], [Message]});
                        {error} ->
                            	Req:respond({404, [], []})
                    end;
                ["crest", "url"|T] ->
		    case crest_peer:spawn_exec(T, Req:parse_post(), Req:recv_body()) of
                        {ok, {CT, Message}} ->
                            Req:respond({200, [{"Content-Type", CT}], [Message]});
                        {ok} ->
                            Req:respond({200, [], []});
                        {error} ->
                            Req:respond({404, [], []})
                    end;
                _ ->
                    Req:serve_file(Path, DocRoot)
            end;
        _ ->
            Req:respond({501, [], []})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.