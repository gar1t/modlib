-module(hello_http).

-include("webapp.hrl").

-export([start/1, request/3]).

%%--------------------------------------------------------------------
%% @doc Starts the hello world server.
%% @spec start(Port) -> {ok, Pid}
%% @end
%%--------------------------------------------------------------------

start(Port) ->
    modlib:start([{port, Port}, {modules, [?MODULE]}]).

%%--------------------------------------------------------------------
%% @doc Handles a webapp request.
%% @spec request(Method, Path, Info) -> Resp
%% @end
%%--------------------------------------------------------------------

request("GET", _Path, _Info) ->
    {ok, {html, "Hello modlib!"}};
request(_Method, _Path, _Info) ->
    {error, "Bad Request"}.
