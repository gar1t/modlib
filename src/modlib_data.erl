-module(modlib_data).

-export([required/2, required/3, optional/3, optional/4]).

required(Name, Params) ->
    required(Name, Params, []).

required(Name, Params, Options) ->
    case lists:keyfind(Name, 1, Params) of
        {_, Val} -> convert(Val, Options, Name);
        false -> throw({badreq, [Name, " is required"]})
    end.

optional(Name, Params, Default) ->
    optional(Name, Params, Default, []).

optional(Name, Params, Default, Options) ->
    case lists:keyfind(Name, 1, Params) of
        {_, Val} -> convert(Val, Options, Name);
        false -> eval_default(Default)
    end.

convert(Val, Options, Name) ->
    case apply_conversion(Val, Options) of
        {ok, Converted} ->
            case apply_range(Converted, Options) of
                {ok, InRange} -> InRange;
                {error, Err} ->
                    throw({badreq, [Name, " is out of range (", Err, ")"]})
            end;
        {error, Err} ->
            throw({badreq, [Name, " is invalid (", Err, ")"]})
    end.

apply_conversion(Val, []) -> {ok, Val};
apply_conversion(Val, [int|_]) when is_list(Val) ->
    try list_to_integer(Val) of
        Int -> {ok, Int}
    catch
        error:badarg -> {error, "not an integer"}
    end;
apply_conversion(Val, [float|_]) when is_list(Val) ->
    try list_to_float(Val) of
        Float -> {ok, Float}
    catch
        error:badarg ->
            try list_to_integer(Val) of
                Int -> {ok, float(Int)}
            catch
                error:badarg ->
                    {error, "not an float"}
            end
    end;
apply_conversion(Val, [bool|_]) when is_list(Val) ->
    case string:to_lower(Val) of
        "" -> {ok, false};
        "0" -> {ok, false};
        "false" -> {ok, false};
        "no" -> {ok, false};
        _ -> {ok, true}
    end;
apply_conversion(Val, [_|Rest]) ->
    apply_conversion(Val, Rest).

apply_range(Val, []) -> {ok, Val};
apply_range(RHS, [{Op, LHS}|Rest]) ->
    case apply_op(Op, RHS, LHS) of
        true -> apply_range(RHS, Rest);
        false -> {error, io_lib:format("must be ~s ~p", [Op, LHS])}
    end;
apply_range(Val, [_|Rest]) ->
    apply_range(Val, Rest).

apply_op('<', L, R) -> L < R;
apply_op('<=', L, R) -> L =< R;
apply_op('=<', L, R) -> L =< R;
apply_op('>', L, R) -> L > R;
apply_op('>=', L, R) -> L >= R;
apply_op('/=', L, R) -> L /= R;
apply_op('<>', L, R) -> L /= R;
apply_op('=', L, R) -> L == R;
apply_op('==', L, R) -> L == R;
apply_op(in, Val, Options) -> lists:member(Val, Options);
apply_op('or', L, Ops) ->
    lists:any(fun({Op, R}) -> apply_op(Op, L, R);
                 (Other) -> exit({invalid_operator, Other})
              end, Ops);
apply_op(Other, _L, _R) -> exit({invalid_operator, Other}).

eval_default({M, F}) -> erlang:apply(M, F, []);
eval_default({M, F, A}) -> erlang:apply(M, F, A);
eval_default(Fun) when is_function(Fun) -> Fun();
eval_default(Val) -> Val.
