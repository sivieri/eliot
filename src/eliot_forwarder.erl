%% @author Alessandro Sivieri <sivieri@elet.polimi.it>
%% @doc Simulator forwarder.
-module(eliot_forwarder).
-behaviour(gen_server).
-include("eliot.hrl").
-export([start_link/0, set_gains/1, get_gain/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {gains}).

% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_gains(Gains) ->
    gen_server:cast(?MODULE, {gains, Gains}).

get_gain(Node1, Node2) ->
    gen_server:call(?MODULE, {gain, Node1, Node2}).

init([]) ->
    {ok, #state{gains = dict:new()}}.

handle_call({gain, Node1, Node2}, _From, State = #state{gains = Gains}) ->
    Reply = gain_value(Node1, Node2, Gains),
    {reply, Reply, State}.

handle_cast({gains, Gains}, _State = #state{gains = OldGains}) ->
    NewGains = dict:merge(fun(_Key, _Value1, Value2) -> Value2 end, OldGains, Gains),
    {noreply, #state{gains = NewGains}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API

gain_value(SourceId, DestId, Gains) ->
    NoiseDb = ?NOISE_AVG + random:uniform() * ?NOISE_DELTA * 2 - ?NOISE_DELTA,
    case dict:is_key({SourceId, DestId}, Gains) of
        true ->
            SignalDb = dict:fetch({SourceId, DestId}, Gains),
            % sig = 10^(sig_dB/10)
            % noise = 10^(noise_dB/10)
            % sig/noise = 10^(sig_dB-noise_dB)/10 --in dB--> (sig_dB-noise_dB) -->
            % sig/noise > sensib (=4dB) --in dB--> (sig_dB-noise_dB) > 4
            if
                (SignalDb - NoiseDb) > ?SENSITIVITY ->
                    % RSSI = Signal+Noise
                    round(10.0 * math:log10(math:pow(10.0, SignalDb / 10.0) + math:pow(10.0, NoiseDb / 10.0)));
                true ->
                    inf
            end;
        % If not key, forget it: probably the module does not have the gains yet
        false ->
            inf
    end.
