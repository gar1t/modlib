-module(modlib_sup).

-export([start_link/0]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, httpd_sup, [httpd_servers()]).

httpd_servers() ->
    case application:get_env(servers) of
        undefined -> [];
        {ok, Servers} ->
            lists:map(fun server_to_httpd_config/1, Servers)
    end.

server_to_httpd_config({Port, Config}) when is_integer(Port) ->
    {httpd, modlib:httpd_config([{port, Port}|Config])}.

