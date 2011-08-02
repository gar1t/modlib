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
    case convert_type(Val, Options) of
        {ok, Converted} ->
            case check_range(Converted, Options) of
                {ok, InRange} -> InRange;
                {error, Err} ->
                    throw({badreq, [Name, " is out of range (", Err, ")"]})
            end;
        {error, Err} ->
            throw({badreq, [Name, " is invalid (", Err, ")"]})
    end.

convert_type(Val, Options) ->
    case proplists:get_bool(int, Options) of
        true -> convert_integer(Val);
        false -> {ok, Val}
    end.

check_range(Val, Options) ->
    case proplists:get_bool(positive, Options) of
        true -> check_positive(Val);
        false -> {ok, Val}
    end.

check_positive(Num) when is_number(Num) ->
    case Num > 0 of
        true -> {ok, Num};
        false -> {error, "must be > 0"}
    end;
check_positive(_Num) ->
    {error, "not a number"}.

convert_integer(Val) ->    
    try list_to_integer(Val) of
        Int -> {ok, Int}
    catch
        error:badarg -> {error, "not an integer"}
    end.

eval_default({M, F}) -> erlang:apply(M, F, []);
eval_default({M, F, A}) -> erlang:apply(M, F, A);
eval_default(Fun) when is_function(Fun) -> Fun();
eval_default(Val) -> Val.
