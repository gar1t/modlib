-module(modlib_webapp).

-include("httpd.hrl").

-export([do/2, behaviour_info/1, parse_transform/2]).

%%%===================================================================
%%% Beahviour API
%%%===================================================================

behaviour_info(callbacks) ->
    [{request, 3}].

%%%===================================================================
%%% Parse transform for webapp behaviors
%%%===================================================================

-define(do_fun_form(BehaviorMod),
        {function,0,do,1,
         [{clause,0,
           [{var,0,'Info'}],
           [],
           [{call,0,
             {remote,0,{atom,0,modlib_webapp},{atom,0,do}},
             [{var,0,'Info'},{atom,0,BehaviorMod}]}]}]}).

parse_transform(Forms, _Options) ->
    {Header, Funs} = split_forms(Forms),
    case is_fun_defined({do, 1}, Funs) of
        true ->
            case is_fun_exported({do, 1}, Header) of
                true -> Forms;
                false -> add_export({do, 1}, Header) ++ Funs
            end;
        false ->
            BehaviorMod = get_module(Header),
            add_export({do, 1}, Header) ++ [?do_fun_form(BehaviorMod)|Funs]
    end.

split_forms(Forms) ->
    split_forms(Forms, [], []).

split_forms([], Header, Funs) ->
    {lists:reverse(Header), lists:reverse(Funs)};
split_forms([H|T], Header, []) ->
    case is_function_form(H) of
        true -> split_forms(T, Header, [H]);
        false -> split_forms(T, [H|Header], [])
    end;
split_forms([H|T], Header, Funs) ->
    split_forms(T, Header, [H|Funs]).

is_function_form({function, _, _, _, _}) -> true;
is_function_form(_) -> false.

is_fun_defined(_, []) -> false;
is_fun_defined({Name, Arity}, [{function, _, Name, Arity, _}|_]) -> true;
is_fun_defined(Fun, [_|T]) -> is_fun_defined(Fun, T).

is_fun_exported(_, []) -> false;
is_fun_exported(Fun, [{attribute, _, export, Exports}|T]) ->
    case lists:member(Fun, Exports) of
        true -> true;
        false -> is_fun_exported(Fun, T)
    end;
is_fun_exported(Fun, [_|T]) -> is_fun_exported(Fun, T).

add_export(Fun, Header) ->
    Header ++ [{attribute, 0, export, [Fun]}].

get_module([]) -> erlang:error(missing_module_attribute);
get_module([{attribute, _, module, Mod}|_]) -> Mod;
get_module([_|T]) -> get_module(T).

%%%===================================================================
%%% httpd mod API wrapper (see http://www.erlang.org/doc/man/httpd.html)
%%%===================================================================

do(#mod{method=Method,
        data=Data,
        request_uri=Uri}=Info, Mod) ->
    case proplists:get_value(status, Data) of
        undefined ->
            case proplists:get_value(response, Data) of
                undefined ->
                    {Path, _, _}=Split = modlib_util:urlsplit_path(Uri),
                    UnquotedPath = modlib_util:unquote(Path),
                    ReqInfo = Info#mod{data=[{'_split_uri', Split}|Data]},
                    try
                        case Mod:request(Method, UnquotedPath, ReqInfo) of
                            {Code, Headers, Resp} when is_integer(Code) ->
                                proceed(Code, Headers, Resp, Data);
                            {Code, Resp} when is_integer(Code) ->
                                proceed(Code, [], Resp, Data);
                            {ok, Resp} ->
                                proceed(200, [], Resp, Data);
                            {redirect, Url} ->
                                proceed(303, [{"Location", Url}], empty, Data);
                            {not_found, Resp} ->
                                proceed(404, [], Resp, Data);
                            {error, Resp} ->
                                proceed(400, [], Resp, Data);
                            not_handled ->
                                {proceed, Data}
                        end
                    catch
                        throw:{badreq, ErrResp} ->
                            proceed(400, [], ErrResp, Data);
                        throw:{forbidden, ErrResp} ->
                            proceed(403, [], ErrResp, Data);
                        Type:Err ->
                            %% NOTE: We're *requiring* that webapps provide
                            %% a fall through handler of not_handled to
                            %% make it clear they're not handling a request.
                            %%
                            %% TODO: write/hookup to httpd logs (it should
                            %% be possible to hook up middleware that
                            %% not only can log to a file but also prepare
                            %% a nicely formatted HTML error report if, say,
                            %% the authenticated user is part of a particular
                            %% group.
                            error_logger:error_report(
                              {webapp_error,
                               {Type, Err, erlang:get_stacktrace()}}),
                            proceed(500, [], {text, "Internal Error"}, Data)
                    end;
                _ ->
                    {proceed, Data}
            end;
        _ ->
            {proceed, Data}
    end.

proceed(Code, Headers, Body, Data) ->
    NewData = [{response, response(Code, Headers, Body)}|Data],
    {proceed, NewData}.

response(Code, H0, {Type, Body}) when is_binary(Body) ->
    response(Code, H0, {Type, [Body]});
response(Code, H0, {Type, Body}) when is_list(Body) ->
    Headers = [{content_type, mime_type(Type)},
               {content_length, body_length(Body)}|H0],
    {response, [{code, Code}|Headers], Body};
response(Code, Headers, empty) ->
    {response, [{code, Code}|Headers], nobody};
response(Code, Headers, Body) when is_list(Body) ->
    response(Code, Headers, {text, Body}).

body_length(Body) when is_list(Body) ->
    integer_to_list(httpd_util:flatlength(Body)).

mime_type(text)  -> "text/plain";
mime_type(html)  -> "text/html";
mime_type(xml)   -> "text/xml";
mime_type(json)  -> "application/json";
mime_type(Type)  -> Type.
