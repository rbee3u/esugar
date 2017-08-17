-module(esugar_bind_transform_tests).
-include_lib("eunit/include/eunit.hrl").
-define(M, esugar_bind_transform_funcs).


esugar_pipe_transform_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
              {<<"function_args">>, fun function_args/0}
            , {<<"multiple_bind">>, fun multiple_bind/0}
            , {<<"fun_named_fun">>, fun fun_named_fun/0}
            , {<<"ifcasereceive">>, fun ifcasereceive/0}
            , {<<"comprehension">>, fun comprehension/0}
        ]
    }.


function_args() ->
    ?assertEqual(ok, ?M:f1(1)),
    ?assertEqual(ok, ?M:f2(1, 1)),
    ?assertEqual(ok, ?M:f2(1, 2)),
    ?assertEqual(ok, ?M:f3(1, 1)),
    ?assertError(function_clause, ?M:f3(1, 2)),
    ?assertEqual(0, ?M:f4(0, 0)),
    ?assertEqual(1, ?M:f4(1, 1)),
    ?assertEqual(2, ?M:f4(1, 2)),
    ?assertEqual(0, ?M:f5(0)),
    ?assertEqual(1, ?M:f5(1)),
    ?assertEqual(-1, ?M:f5(-1)),
    ?assertEqual(ok, ?M:f6(0)),
    ?assertEqual(#{x => 1}, ?M:f6({1, #{x => 1}})),
    ?assertEqual(2, ?M:f6({1, #{x => 2}})),
    ok.


multiple_bind() ->
    ?assertEqual({2, 2}, ?M:f7()),
    ?assertEqual({2, 2}, ?M:f8()),
    ok.


fun_named_fun() ->
    ?assertEqual(1, ?M:f9()),
    ?assertEqual({1, 2}, ?M:f10()),
    ?assertEqual({1, 3}, ?M:f11()),
    ?assertEqual({1, 2}, ?M:f12()),
    ok.


ifcasereceive() ->
    ?assertEqual(0, ?M:f13(1)),
    ?assertEqual({1, 1}, ?M:f14(1)),
    ?assertEqual({3, 1}, ?M:f14(2)),
    ?assertEqual(-1, ?M:f15(11)),
    ?assertEqual($2, ?M:f15(21)),
    ?assertEqual({go, $1}, ?M:f15(1)),
    ?assertEqual(undef, ?M:f15(123)),
    ?assertEqual({error,badarg}, ?M:f15(zz)),
    ok.


comprehension() ->
    ?assertEqual([1,2,3,4,5], ?M:f16(0)),
    ?assertEqual([2,3,4,5], ?M:f16(1)),
    ?assertEqual({4, [1,2,3,4,5]}, ?M:f17(0)),
    ?assertEqual({4, [2,3,4,5]}, ?M:f17(1)),
    ok.

