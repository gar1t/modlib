-module(proxy_http).

-include("webapp.hrl").
-include("httpd.hrl").

-export([start/1, request/3]).

start(Port) ->
    application:start(inets),
    modlib:start([{port, Port}, {modules, [?MODULE]}]).

request(_, "/favicon.ico", _) ->
    {not_found, "Not Found"};
request(_Method, _Path, Mod) ->
    handle_request(Mod).

handle_request(Mod) ->
    Method = httpc_method(Mod),
    URL = proxy_url(httpc_uri(Mod)),
    {Headers, ContentType} = httpc_headers(Mod),
    Body = httpc_body(Mod),
    handle_response(httpc_request(Method, URL, Headers, ContentType, Body)).

httpc_request(Method, URL, Headers, undefined, _) ->
    httpc_request(Method, {URL, Headers});
httpc_request(Method, URL, Headers, ContentType, Body) ->
    httpc_request(Method, {URL, Headers, ContentType, Body}).

httpc_request(Method, Request) ->
    error_logger:info_report({proxy_request, {Method, Request}}),
    httpc:request(Method, Request, [], []).

handle_response({ok, {{_, Code, _}, Headers, Body}}) ->
    {Code, Headers, Body};
handle_response({error, Err}) ->
    error_logger:error_report({proxy_error, Err}),
    {500, "Internal Error"}.

proxy_url(URI) ->
    "http://localhost:8888" ++ URI.

httpc_method(#mod{method="POST"}) -> post;
httpc_method(#mod{method="GET"}) -> get;
httpc_method(#mod{method="DELETE"}) -> delete;
httpc_method(#mod{method="PUT"}) -> put;
httpc_method(#mod{method="HEAD"}) -> head;
httpc_method(_) -> bad_request().

httpc_uri(#mod{request_uri=URI}) -> URI.

httpc_headers(#mod{parsed_header=Headers}) ->
    case lists:keytake("content-type", 1, Headers) of
        {value, {_, ContentType}, NewHeaders} ->
            {NewHeaders, ContentType};
        false ->
            {Headers, undefined}
    end.

httpc_body(#mod{entity_body=Body}) -> Body.

bad_request() ->
    throw({badreq, "Bad Request"}).
