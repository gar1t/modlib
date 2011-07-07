-module(echo_http).

-include_lib("modlib/include/webapp.hrl").

-export([start/1, request/3]).

-define(TITLE, "Echo").

start(Port) ->
    modlib:start([{port, Port}, {modules, [?MODULE]}]).

request(Method, Path, Info) ->
    {ok, 
     {html, 
      ["<html>",
       "<head><title>", ?TITLE, "</title></head>",
       "<body>",
       request_line(Method, Path),
       query_params(Info),
       post_params(Info),
       headers(Info),
       "</body>",
       "</html>"]
     }}.

request_line(Method, Path) ->
    ["<p>", Method, " ", Path, "</p>"].

query_params(Info) ->
    case modlib:parse_qs(Info) of
        [] -> [];
        Params ->
            ["<h4>Query Params</h4>",
             [["<div><b>", Name, "</b>: ", Value, "</div>"]
              || {Name, Value} <- Params]]
    end.

post_params(Info) ->
    case modlib:parse_post(Info) of
        {error, content_type} -> [];
        {ok, []} -> [];
        {ok, Params} ->
            ["<h4>Post Params</h4>",
             [["<div><b>", Name, "</b>: ", Value, "</div>"]
              || {Name, Value} <- Params]]
    end.

headers(Info) ->
    ["<h4>Headers</h4>",
     [["<div><b>", Name, "</b>: ", Value, "</div>"] 
      || {Name, Value} <- modlib:headers(Info)]].
