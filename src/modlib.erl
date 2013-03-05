-module(modlib).

-include("httpd.hrl").

-export([start/1, services/0]).

-export([httpd_config/1]).

-export([parse_qs/1,
	 parse_body/1,
	 remote_user/1,
	 body/1,
	 headers/1,
	 header/2,
	 header/3]).

-define(DEFAULT_SERVER_ROOT, ".").
-define(DEFAULT_DOCUMENT_ROOT, ".").

%%--------------------------------------------------------------------
%% @doc Starts a modlib server.
%% @spec start(modlib_config()) -> {ok, Pid} | {error, Reason}
%% @end
%%--------------------------------------------------------------------

start(ModlibConfig) ->
    inets:start(httpd, httpd_config(ModlibConfig)).

%%--------------------------------------------------------------------
%% @doc Returns a list of httpd services.
%%
%% TODO: Do we need to tag the "service"? This isn't really abstract.
%% We're talking httpd services.
%%
%% @spec services() -> [{httpd, pid()}]
%% @end
%%--------------------------------------------------------------------

services() ->
    [{httpd, ChildPid} || {_, ChildPid, _, _}
                              <- supervisor:which_children(modlib_sup)].

%%--------------------------------------------------------------------
%% @doc Returns a propery list of parsed query string params.
%% @spec parse_qs(Info) -> [{Name, Value}]
%% @end
%%--------------------------------------------------------------------

parse_qs(#mod{data=Data, request_uri=Uri}) ->
    case proplists:get_value('_split_uri', Data) of
        {_Path, Qs, _Frag} -> modlib_util:parse_qs(Qs);
        _ ->
            {_Path, Qs, _Frag} = modlib_util:urlsplit_path(Uri),
            modlib_util:parse_qs(Qs)
    end.

%%--------------------------------------------------------------------
%% @doc Attempts to parse the body as a form-urlencoded value.
%%
%% Returns {error, content_type} if the content type is not form
%% urlencoded.
%%
%% @spec parse_body(Info) -> {ok, proplist()} | {error, Reason}
%% Reason = content_type
%% @end
%%--------------------------------------------------------------------

parse_body(#mod{parsed_header=Header, entity_body=Body}) ->
    case proplists:get_value("content-type", Header) of
        "application/x-www-form-urlencoded"++_ ->
            {ok, modlib_util:parse_qs(Body)};
        _ -> {error, content_type}
    end.

%%--------------------------------------------------------------------
%% @doc Returns a validated inets httpd config for a modlib config.
%% @spec httpd_config(modlib_config()) -> httpd_config()
%% @end
%%--------------------------------------------------------------------

httpd_config(ModlibConfig) ->
    apply_httpd_config_defaults(
      validate_httpd_config(
        httpd_config(ModlibConfig, []))).

%%--------------------------------------------------------------------
%% @doc Translated modlib config to httpd config.
%% @spec http_config(modlib_config(), httpd_config()) -> httpd_config()
%% @end
%%--------------------------------------------------------------------

httpd_config([], Acc) -> Acc;
httpd_config([{modules, Mods}|T], Acc) ->
    httpd_config(T, [{modules, wrap_webapps(Mods)}|Acc]);
httpd_config([Option|T], Acc) ->
    httpd_config(T, [Option|Acc]).

%%--------------------------------------------------------------------
%% @doc Raises an error if there are problems with Config.
%% @spec validate_httpd_config(httpd_config()) -> ok
%% @end
%%--------------------------------------------------------------------

validate_httpd_config(HttpdConfig) ->
    validate_minimum_httpd_config(HttpdConfig),
    %% We'll allow invalid values here - they'll be caught by httpd.
    HttpdConfig.

%%--------------------------------------------------------------------
%% @doc Raises an error if HttpdConfig is missing any required options.
%% @spec validate_required_httpd_config(httpd_config()) -> ok
%% @end
%%--------------------------------------------------------------------

validate_minimum_httpd_config(HttpdConfig) ->
    require_httpd_option(port, HttpdConfig),
    require_httpd_option(modules, HttpdConfig).

%%--------------------------------------------------------------------
%% @doc Raises an error if Option isn't in HttpdConfig.
%% @spec require_httpd_option(atom(), httpd_config()) -> ok
%% @end
%%--------------------------------------------------------------------

require_httpd_option(Option, HttpdConfig) ->
    case lists:keyfind(Option, 1, HttpdConfig) of
        false -> erlang:error({required_option, Option});
        _ -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Ensures that any modlib_webapp behavior modules are returned
%% as parameterized modules.
%% @spec wrap_webapps([module()]) -> [module()]
%% @end
%%--------------------------------------------------------------------

wrap_webapps(Mods) ->
    lists:map(
      fun(Mod) ->
              case is_webapp(Mod) of
                  true -> modlib_webapp:new(Mod);
                  false -> Mod
              end
      end, Mods).

%%--------------------------------------------------------------------
%% @doc Returns true if a module is a modlib_webapp behavior.
%% @spec is_webapp(module()) -> boolean()
%% @end
%%--------------------------------------------------------------------

is_webapp(Mod) ->
    Behaviors = [Value || {Name, Value} <- Mod:module_info(attributes),
                          Name == behavior orelse Name == behaviour],
    lists:any(fun(M) -> M == [modlib_webapp] end, Behaviors).

%%--------------------------------------------------------------------
%% @doc Adds default httpd config values for any missing.
%% @spec apply_httpd_config_defaults(httpd_config()) -> httpd_config()
%% @end
%%--------------------------------------------------------------------

apply_httpd_config_defaults(HttpdConfig) ->
    ServerRoot = proplists:get_value(server_root, HttpdConfig,
                                     ?DEFAULT_SERVER_ROOT),
    DocRoot = proplists:get_value(document_root, HttpdConfig,
                                  ?DEFAULT_DOCUMENT_ROOT),
    apply_config_defaults([{server_name, ""},
                           {server_root, ServerRoot},
                           {document_root, DocRoot}], HttpdConfig).

%%--------------------------------------------------------------------
%% @doc Adds default values to config for any missing value.
%% @spec apply_config_defaults(Defaults, Config1) -> Config2
%% Defaults = proplist()
%% Config1 = proplist()
%% Config2 = proplist()
%% @end
%%--------------------------------------------------------------------

apply_config_defaults([], Config) ->
    Config;
apply_config_defaults([{Name, DefaultVal}|T], Config) ->
    case proplists:get_value(Name, Config) of
        undefined ->
            apply_config_defaults(T, [{Name, DefaultVal}|Config]);
        _ ->
            apply_config_defaults(T, Config)
    end.

%%--------------------------------------------------------------------
%% @doc Raises an error if Path is not a valid directory.
%% @spec validate_dir(string()) -> ok
%% @end
%%--------------------------------------------------------------------

validate_dir(Path) ->
    case filelib:is_dir(Path) of
        true -> ok;
        false -> erlang:error({bad_dir, Path})
    end.

%%--------------------------------------------------------------------
%% @doc Return remote user associated with a request.
%%
%% Returns undefined if there is not remote user.
%%
%% @spec remote_user(Info) -> string() | undefined
%% @end
%%--------------------------------------------------------------------

remote_user(#mod{data=Data}) ->
    proplists:get_value(remote_user, Data).


%%--------------------------------------------------------------------
%% @doc Returns the request unmodified body.
%% @spec body(Info) -> binary()
%% @end
%%--------------------------------------------------------------------

body(#mod{entity_body=Body}) -> Body.

%%--------------------------------------------------------------------
%% @doc Returns parsed headers.
%% @spec headers(Info) -> [{Name, Value}]
%% @end
%%--------------------------------------------------------------------

headers(#mod{parsed_header=H}) -> H.

%%--------------------------------------------------------------------
%% @doc Return a header value.
%%
%% Returns undefined if the named value doesn't exist.
%%
%% @spec header(Name, Info) -> string() | undefined
%% @end
%%--------------------------------------------------------------------

header(Name, #mod{parsed_header=Hs}) ->
    proplists:get_value(Name, Hs).

%%--------------------------------------------------------------------
%% @doc Return a header value.
%%
%% Returns Default if the named value doesn't exist.
%%
%% @spec header(Name, Info, Default) -> string() | Default
%% @end
%%--------------------------------------------------------------------

header(Name, #mod{parsed_header=Hs}, Default) ->
    proplists:get_value(Name, Hs, Default).
