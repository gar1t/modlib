-module(modlib_dtl).

-export([render/2, compile_and_render/2]).

render(Template, Vars) when is_list(Template) ->
    render(list_to_atom(Template), Vars);
render(Template, Vars) when is_atom(Template) ->
    try Template:render(Vars) of
        {ok, Bin} -> Bin;
        {error, Err} -> render_error(Template, Err)
    catch
        error:undef ->
            compile_and_render(Template, Vars)
    end.

render_error(Template, Err) ->
    error({dtl_render_error, Err, Template}).

compile_and_render(Template, Vars) when is_list(Template) ->
    compile_and_render(list_to_atom(Template), Vars);
compile_and_render(Template, Vars) when is_atom(Template) ->
    Source = template_source(Template),
    compile(Source, Template),
    case Template:render(Vars) of
        {ok, Bin} -> Bin;
        {error, Err} -> render_error(Template, Err)
    end.

compile(Source, Template) when is_atom(Template) ->
    case erlydtl:compile(Source, Template) of
        ok  -> ok;
        {error, Err} -> compile_error(Err, Template)
    end.

compile_error(Err, Template) ->
    error({dtl_compile_error, Err, Template}).

template_source(Template) ->
    filename:join(template_base_dir(), atom_to_list(Template)).

template_base_dir() ->
    try_template_base_dir([app_env, os_env]).

try_template_base_dir([]) -> default_template_base_dir();
try_template_base_dir([app_env|Rest]) ->
    case application:get_env(dtl_base_dir) of
        {ok, Dir} -> Dir;
        undefined -> try_template_base_dir(Rest)
    end;
try_template_base_dir([os_env|Rest]) ->
    case os:getenv("DTL_BASE_DIR") of
        false -> try_template_base_dir(Rest);
        Dir -> Dir
    end.

default_template_base_dir() ->
    filename:join([".", "priv", "dtl"]).

