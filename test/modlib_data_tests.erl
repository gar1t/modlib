-module(modlib_data_tests).

-include_lib("eunit/include/eunit.hrl").

-import(modlib_data, [optional/4]).

optional_test() ->
    Params = [{"s", "Foo"},
              {"i", "123"},
              {"f", "123.456"},
              {"b1", ""},
              {"b2", "no"},
              {"b3", "No"},
              {"b4", "FALSE"},
              {"b5", "0"},
              {"b6", "Yes"}],

    %% Defaults
    ?assertEqual(nil, optional("foo", Params, nil, [])),
    ?assertEqual(nil, optional("foo", Params, nil, [int])),

    %% Strings are returns if type isn't specified
    ?assertEqual("Foo", optional("s", Params, nil, [])),
    ?assertEqual("123", optional("i", Params, nil, [])),
    ?assertEqual("123.456", optional("f", Params, nil, [])),

    %% Integers
    ?assertEqual(123, optional("i", Params, nil, [int])),
    ?assertThrow({badreq, _}, optional("s", Params, nil, [int])),
    ?assertThrow({badreq, _}, optional("f", Params, nil, [int])),
    ?assertThrow({badreq, _}, optional("b1", Params, nil, [int])),

    %% Floats
    ?assertEqual(123.456, optional("f", Params, nil, [float])),
    ?assertEqual(123.0, optional("i", Params, nil, [float])),
    ?assertThrow({badreq, _}, optional("s", Params, nil, [float])),
    ?assertThrow({badreq, _}, optional("b1", Params, nil, [float])),

    %% Booleans
    ?assertEqual(false, optional("b1", Params, nil, [bool])),
    ?assertEqual(false, optional("b2", Params, nil, [bool])),
    ?assertEqual(false, optional("b3", Params, nil, [bool])),
    ?assertEqual(false, optional("b4", Params, nil, [bool])),
    ?assertEqual(false, optional("b5", Params, nil, [bool])),
    ?assertEqual(true, optional("b6", Params, nil, [bool])),
    ?assertEqual(true, optional("s", Params, nil, [bool])),
    ?assertEqual(true, optional("i", Params, nil, [bool])),
    ?assertEqual(true, optional("f", Params, nil, [bool])),

    %% Range
    ?assertEqual(123, optional("i", Params, nil, [int, {'>', 0}])),
    ?assertEqual(123, optional("i", Params, nil, [int, {'==', 123}])),
    ?assertEqual(123.0, optional("i", Params, nil, [float, {'<=', 123}])),
    ?assertThrow({badreq, _}, optional("i", Params, nil, [int, {'<', 123}])),
    ?assertEqual("Foo", optional("s", Params, nil, [{'=', "Foo"}])),
    ?assertEqual("Foo", optional("s", Params, nil, [{'<>', "Bar"}])),
    ?assertThrow({badreq, _}, optional("s", Params, nil, [{'=', "Boo"}])),
    ?assertEqual("Foo", optional("s", Params, nil, [{in, ["Foo", "Bar"]}])),
    ?assertThrow({badreq, _}, optional("s", Params, nil,
                                       [{in, ["Bar", "Baz"]}])),
    ?assertEqual("123", optional("i", Params, nil, [{'=<', "123"}])),
    ?assertEqual("123", optional("i", Params, nil, [{'=<', "124"}])),
    ?assertEqual(123.456, optional("f", Params, nil,
                                   [float, {'>', 123}, {'<', 124}])),

    ?assertEqual(123.456, optional("f", Params, nil,
                                   [float, {'or', [{'<', 124}, {'>', 125}]}])),

    %% Bad operators
    ?assertExit({invalid_operator, badop},
                optional("i", Params, nil, [{badop, "123"}])),
    ?assertExit({invalid_operator, badop},
                optional("i", Params, nil, [{'or', [{badop, "123"}]}])),

    ok.
