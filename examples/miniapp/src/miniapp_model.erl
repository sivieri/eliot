-module(miniapp_model).
-export([start_link/0, model/0]).
-include("scenario.hrl").

% Public API

start_link() ->
    Pid = spawn_link(?MODULE, model, []),
    {ok, Pid}.

model() ->
    receive
        {Sender, data} ->
            Sender ! {self(), params()},
            model();
        {Sender, {eval, Cur, Params}} ->
            Val = case application:get_env(miniapp, type) of
                {ok, ac} ->
                    1000;
                {ok, dw} ->
                    1500;
                {ok, wm} ->
                    1500
            end,
            #parameter{name = starttime, value = Start} = hd(lists:filter(fun(#parameter{name = starttime}) -> true;
                                                                                                         (_Parameter) -> false end, Params)),
            #parameter{name = endtime, value = End} = hd(lists:filter(fun(#parameter{name = endtime}) -> true;
                                                                                                         (_Parameter) -> false end, Params)),
            if
                Cur >= Start andalso Cur < End ->
                    Sender ! {self(), Val};
                true ->
                    Sender ! {self(), 0}
            end,
            model();
        Any ->
            io:format("Appliance model: Unknown message ~p~n", [Any]),
            model()
    end.

% Private API

params() ->
    [#parameter{name = starttime, type = time}, #parameter{name = endtime, type = time, value = 1}].
