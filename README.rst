======
modlib
======

modlib is a small library that makes it easy to write 'mod' plugins for
Erlang's httpd server.

Motivation
==========

The standard Erlang language environment ships with a powerful set of network
tools in the 'inets' OTP application. This includes a full featured HTTP server
in the ``httpd module``_.

.. _httpd module: http://www.erlang.org/doc/man/httpd.html

``httpd`` supports a plugin model that's similar to Apache's 'mod' API. It also
ships with a number of useful mods, including support for authentication,
authorization, URL aliases, and several others.

modlib makes it drop dead simple to write extensions for httpd, including
Erlang based web apps.

Sample Code
===========

A simple hello world app::

  -module(hello_http).

  -include_lib("modlib/include/webapp.hrl").

  -export([start/1, request/3]).

  start(Port) ->
      modlib:start([{port, Port}, {modules, [?MODULE]}]).

  request("GET", _Path, _Info) ->
      {ok, {html, "Hello modlib!"}};
  request(_Method, _Path, _Info) ->
      {error, "Bad Request"}.

A more complex app (illustrates more modlib features and how Erlang can be used
to elegantly structure your HTTP handlers)::

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
      case modlib:parse_body(Info) of
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

Important notes:

 - You must include "webapp.hr" in your webapp modules (see above)

 - modlib doesn't come with template support - but erlydtl is recommended as
   it's outstanding

 - modlib wraps the httpd mod interface - while you don't need to know httpd
   inside and out, it won't hurt to read the docs

 - Refer to "Webapp Cheat Sheet" below for more details

Webapp Cheat Sheet
==================

A "web app" is an httpd mod that implements ``request/3``, which is called with
the HTTP request details and returns the HTTP response.

The only "magic" here is that the required include of webapp.hrl uses a parse
transform to wrap the rather complex modlib API.

The request method looks like this::

  @spec
  request(Method, Path, Info) -> Response
      Method = "GET" | "POST" | "PUT" | "DELETE" | "HEAD"
      Path = string()
      Info = #mod{}
      Response = {ok, Content} |
                 {error, Content} |
                 {not_found, Content} |
                 {redirect, Location} |
                 {Code, Content} |
                 {Code, Headers, Content} |
                 not_handled
      Content = iolist() | {Type, iolist()}
      Type = text | html | xml | json
      Location = string()
      Code = integer()
      Headers = [{string(), string()}]
  @end

Refer to ``include/httpd.hrl`` for details on the ``mod`` record (typically not
used, but needed for some cases).

The Path does not contain query string or reference elements. The original
request URL is in the ``request_url`` element of Info#mod.

Use ``modlib:parse_qs(Info)`` to return a proplist of query string params.

Use ``modlib:parse_body(Info)`` to return a proplist of form-urlencoded body
params. (Note that the content type in the request headers must be
"application/x-www-form-urlencoded" otherwise parse_body/1 will return
``{error, content_type}``.)

Return ``not_handled`` from ``request/3`` to let httpd continue processing the
request with downstream modules.

modlib applications can be started using ``modlib:start/1`` or (more typically)
by starting the modlib OTP application with the appropriate config.

Here's a sample modlib config for a non-SSL app::

  {modlib,
   [{servers,
     [{8080,
       [{server_root, "/some/dir"},
        {document_root, "/some/dir/htdocs"},
        {modules, [my_modlib_webapp, mod_get]},
        {mime_types, [{"css", "text/css"},
                      {"js", "text/javascript"},
                      {"gif", "image/gif"},
                      {"jpeg", "image/jpeg"},
                      {"png", "image/png"}]}]}]}]}

Here's a sample modlib config for an SSL enabled app (also includes the use of
mod_auth for protecting a directory with basic auth)::

  {modlib,
   [{servers,
     [{443,
       [{server_root, "/some/dir"},
        {document_root, "/some/dir/htdocs"},
        {socket_type, essl},
        {ssl_certificate_file, "/some/dir/server.crt"},
        {ssl_certificate_key_file, "/some/dir/server.key"},
        {ssl_ca_certificate_file, "/some/dir/ca.crt"},
        {modules,[mod_auth, my_modlib_webapp, mod_get2]},
        {directory, {"/protected",
                    [{auth_type, plain},
                     {auth_user_file, "/some/dir/users"},
                     {auth_group_file, "/some/dir/groups"},
                     {auth_name, "My Modlib Webapp"},
                     {require_group, ["users"]}]}},
        {mime_types,[{"css", "text/css"},
                     {"js", "text/javascript"},
                     {"gif", "image/gif"},
                     {"jpeg", "image/jpeg"},
                     {"png", "image/png"}]}]}]}]}

The configuration proplist for the server port is identical to the httpd
configration documented in http://www.erlang.org/doc/man/httpd.html.

``mod_get2`` is a copy of ``mod_get`` with support for etags.

For template support, use the excellent Django language implementation at
https://github.com/evanmiller/erlydtl.

To Do
=====

- Need a "catch all" handler that doesn't barf with 500 when httpd runs out of
  modules (e.g. there's a single webapp that returns not_handled as its catch
  all).

- If modlib_webapp is declared as a behavior, it breaks the wrapping that's
  done by the webapp include.

- Remove mochiweb util module dependencies, lazy!

- Support for easy direct-to-socket responses (i.e. already_sent pattern)

- How is keep-alive / long running connections supported?


inets Wish List
===============

- Simplify the list of the options that are needed in inets:start/2
