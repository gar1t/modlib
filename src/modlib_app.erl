-module(modlib_app).

-behavior(application).

-export([start/2, stop/1]).

start(normal, []) ->
    modlib_sup:start_link().

stop(_State) ->
    ok.
