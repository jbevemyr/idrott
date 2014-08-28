%    -*- Erlang -*-

-module('idrott_json2').
-author('jb@bevemyr.com').

-export([encode_pretty/1]).

encode_pretty(Term) ->
    encode(Term, _Indent=0).

encode(true, _Indent) -> "true";
encode(false, _Indent) -> "false";
encode(null, _Indent) -> "null";
encode(undefined, _Indent) -> "null";
encode(B, _Indent) when is_binary(B) -> encode_string(B);
encode(I, _Indent) when is_integer(I) -> integer_to_list(I);
encode(F, _Indent) when is_float(F) -> float_to_list(F);
encode(L, Indent) when is_list(L) ->
    case is_string(L) of
        yes -> encode_string(L);
        unicode -> encode_string(xmerl_ucs:to_utf8(L));
        no -> encode({array, L}, Indent)
    end;
encode({array, Props}, Indent) when is_list(Props) ->
    encode_array(Props, Indent);
encode({struct, Props} = T, Indent) when is_list(Props) ->
    encode_object(T, Indent);
encode(Bad, _Indent) -> exit({json_encode, {bad_term, Bad}}).

%% Encode an Erlang string to JSON.
%% Accumulate strings in reverse.

encode_string(B) when is_binary(B) -> encode_string(binary_to_list(B));
encode_string(S) -> encode_string(S, [$"]).

encode_string([], Acc) -> lists:reverse([$" | Acc]);
encode_string([C | Cs], Acc) ->
    case C of
        $" -> encode_string(Cs, [$", $\\ | Acc]);
        % (don't escape solidus on encode)
        $\\ -> encode_string(Cs, [$\\, $\\ | Acc]);
        $\b -> encode_string(Cs, [$b, $\\ | Acc]);      % note missing \
        $\f -> encode_string(Cs, [$f, $\\ | Acc]);
        $\n -> encode_string(Cs, [$n, $\\ | Acc]);
        $\r -> encode_string(Cs, [$r, $\\ | Acc]);
        $\t -> encode_string(Cs, [$t, $\\ | Acc]);
        C when C >= 0, C < $\s ->
            % Control characters must be unicode-encoded.
            Hex = lists:flatten(io_lib:format("~4.16.0b", [C])),
            encode_string(Cs, lists:reverse(Hex) ++ "u\\" ++ Acc); % "
        C when C =< 16#FFFF -> encode_string(Cs, [C | Acc]);
        _ -> exit({json_encode, {bad_char, C}})
    end.

%% Encode an Erlang object as a JSON object, allowing string or atom keys.
%% Note that order is irrelevant in both internal and external object
%% representations.  Nevertheless, the output will respect the order
%% of the input.

encode_object({struct, _Props} = Obj, Indent) ->
    M = obj_fold(fun({Key, Value}, Acc) ->
        S = case Key of
                B when is_binary(B) -> encode_string(B);
                L when is_list(L) ->
                    case is_string(L) of
                        yes -> encode_string(L);
                        unicode -> encode_string(xmerl_ucs:to_utf8(L));
                        no -> exit({json_encode, {bad_key, Key}})
                    end;
                A when is_atom(A) -> encode_string(atom_to_list(A));
                _ -> exit({json_encode, {bad_key, Key}})
            end,
        NewI = Indent+1+length(lists:flatten(S))+1,
        V = encode(Value, NewI),
        case Acc of
            [] -> [indent(Indent+1), S, $:, V];
            _ -> [Acc, $,, $\n, indent(Indent+1), S, $:, V]
        end
    end, [], Obj),
    [${, $\n, M, $\n, indent(Indent), $}].

%% Encode an Erlang tuple as a JSON array.
%% Order *is* significant in a JSON array!

encode_array(T, Indent) ->
    M = lists:foldl(fun(E, Acc) ->
        V = encode(E, Indent+1),
        case Acc of
            [] -> [indent(Indent+1),V];
            _ -> [Acc, $,,$\n, indent(Indent+1), V]
        end
    end, [], T),
    [$[, $\n, M, $\n, indent(Indent), $]].

indent(N) ->
    lists:duplicate(N, $\ ).

obj_fold(Fun, Acc, {struct, Props}) ->
    lists:foldl(Fun, Acc, Props).


is_string([]) -> yes;
is_string(List) -> is_string(List, non_unicode).

is_string([C|Rest], non_unicode) when is_integer(C), C >= 0, C =< 255 ->
    is_string(Rest, non_unicode);
is_string([C|Rest], _) when is_integer(C), C =< 65000 -> is_string(Rest, unicode);
is_string([], non_unicode) -> yes;
is_string([], unicode) -> unicode;
is_string(_, _) -> no.
