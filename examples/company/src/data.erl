-module(data).
-export([encode_params/1, decode_params/1, decode_name/1, encode_slots/1, decode_slots/1]).
-define(PADDING, 20).
-include("scenario.hrl").
-compile(export_all).

% Public API

encode_params(Params) ->
    lists:foldl(fun(#parameter{name = Name, type = Type, value = Value, fixed = true}, AccIn) -> 
                            Bin1 = add_atom(Name),
                            Cur = <<Bin1/binary, Type:8/unsigned-little-integer, Value:8/unsigned-little-integer, 1:8/unsigned-little-integer>>,
                            <<AccIn/binary, Cur/binary>>;
                         (#parameter{name = Name, type = Type, value = Value, fixed = false}, AccIn) ->
                            Bin1 = add_atom(Name),
                            Cur = <<Bin1/binary, Type:8/unsigned-little-integer, Value:8/unsigned-little-integer, 0:8/unsigned-little-integer>>,
                            <<AccIn/binary, Cur/binary>> end, <<>>, Params).

decode_params(Other) ->
    decode_params(Other, []).

encode_slots(Slots) ->
    lists:foldl(fun(#slot{starttime = {SH, _SM}, endtime = {EH, _EM}, priority = Priority}, AccIn) ->
                            Cur = <<SH:8/unsigned-little-integer, EH:8/unsigned-little-integer, Priority:8/unsigned-little-integer>>,
                            <<AccIn/binary, Cur/binary>> end, <<>>, Slots).

decode_slots(Other) ->
    decode_slots(Other, []).

decode_name(Name) ->
    remove_atom(Name).

% Private API

decode_params(<<>>, AccIn) ->
    lists:reverse(AccIn);
decode_params(<<L1:8, NameBin:L1/binary, Type:8/unsigned-little-integer, Value:8/unsigned-little-integer, 1:8/unsigned-little-integer, Other/binary>>, AccIn) ->
    decode_params(Other, [#parameter{name = remove_atom(NameBin), type = Type, value = Value, fixed = true}|AccIn]);
decode_params(<<L1:8, NameBin:L1/binary, Type:8/unsigned-little-integer, Value:8/unsigned-little-integer, 0:8/unsigned-little-integer, Other/binary>>, AccIn) ->
    decode_params(Other, [#parameter{name = remove_atom(NameBin), type = Type, value = Value, fixed = false}|AccIn]).

decode_slots(<<>>, AccIn) ->
    lists:reverse(AccIn);
decode_slots(<<SH:8/unsigned-little-integer, EH:8/unsigned-little-integer, Priority:8/unsigned-little-integer, Other/binary>>, AccIn) ->
    decode_slots(Other, [#slot{starttime = {SH, 00}, endtime = {EH, 00}, priority = Priority}|AccIn]).

add_atom(Atom) ->
    String = erlang:atom_to_list(Atom),
    Length = length(String),
    Bin = erlang:list_to_binary(String),
    <<Length:8, Bin/binary>>.

remove_atom(Binary) ->
    erlang:list_to_atom(erlang:binary_to_list(Binary)).

