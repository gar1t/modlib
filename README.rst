======
modlib
======

modlib is a small library that makes it easy to write 'mod' plugins for
Erlang's httpd server.

Motivation
==========

The standard Erlang language environment ships with a powerful set of network
tools in the 'inets' OTP application. This includes a full featured HTTP server
in the `httpd module`_.

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

  wrequest("GET", _Path, _Info) ->
      {ok, {html, "Hello modlib!"}};
  request(_Method, _Path, _Info) ->
      {error, "Bad Request"}.

To Do
=====

- Need a "catch all" handler that doesn't barf with 500 when httpd runs out of
  modules (e.g. there's a single webapp that returns not_handled as its catch
  all).

- If modlib_webapp is declared as a behavior, it breaks the wrapping that's
  done by the webapp include.

- Remove mochiweb util module dependencies, lazy!

inets Wish List
===============

- Simplify the list of the options that are needed in inets:start/2
