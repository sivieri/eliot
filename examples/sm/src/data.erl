-module(data).
-export([encode_params/1, decode_params/1]).
-define(PADDING, 20).
-include("scenario.hrl").
-compile(export_all).

% Public API

encode_params(Params) ->
    lists:foldl(fun(#parameter{name = Name, type = Type, value = Value, fixed = true}, AccIn) -> 
                            Bin1 = add_padding_atom(Name),
                            Bin2 = add_padding_atom(Type),
                            Cur = <<Bin1/binary, Bin2/binary, Value:8/unsigned-little-integer, 1:8/unsigned-little-integer>>,
                            <<AccIn/binary, Cur/binary>>;
                         (#parameter{name = Name, type = Type, value = Value, fixed = false}, AccIn) ->
                            Bin1 = add_padding_atom(Name),
                            Bin2 = add_padding_atom(Type),
                            Cur = <<Bin1/binary, Bin2/binary, Value:8/unsigned-little-integer, 0:8/unsigned-little-integer>>,
                            <<AccIn/binary, Cur/binary>> end, <<>>, Params).

decode_params(Other) ->
    decode_params(Other, []).

% Private API

decode_params(<<>>, AccIn) ->
    lists:reverse(AccIn);
decode_params(<<NameBin:20/binary, TypeBin:20/binary, Value:8/unsigned-little-integer, 1:8/unsigned-little-integer, Other/binary>>, AccIn) ->
    decode_params(Other, [#parameter{name = remove_padding_atom(NameBin), type = remove_padding_atom(TypeBin), value = Value, fixed = true}|AccIn]);
decode_params(<<NameBin:20/binary, TypeBin:20/binary, Value:8/unsigned-little-integer, 0:8/unsigned-little-integer, Other/binary>>, AccIn) ->
    decode_params(Other, [#parameter{name = remove_padding_atom(NameBin), type = remove_padding_atom(TypeBin), value = Value, fixed = false}|AccIn]).

add_padding_atom(Atom) ->
    String = erlang:atom_to_list(Atom),
    erlang:list_to_binary(string:right(String, ?PADDING, $0)).

remove_padding_atom(Binary) ->
    List = erlang:binary_to_list(Binary),
    erlang:list_to_atom(string:strip(List, left, $0)).
