-module(esugar_import_as_transform_tests).
-compile([{parse_transform, esugar_import_as_transform}]).
-include_lib("eunit/include/eunit.hrl").
-import_as({lists, [{seq/2, ls}, {seq/3, ls}]}).
-import_as({lists, [{sum/1, sum}]}).
-import_as({lists, [{reverse/1, rev}]}).


esugar_import_as_transform_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
              {<<"test_import_as">>, fun test_import_as/0}
        ]
    }.


test_import_as() ->
    L = rev(lists:seq(1, 10)),
    L = lists:reverse(ls(1, 10)),
    L = rev(ls(1, 10)),
    Fun1 = fun(X) -> rev(X) end,
    [c, b, a] = Fun1([a, b, c]),
    Fun2 = fun rev/1,
    [b, a] = Fun2([a, b]),
    [1, 3, 5, 7, 9] = ls(1, 10, 2),
    25 = sum(rev(ls(1, 10, 2))).

