-module(esugar_do_transform_tests).
-compile([{parse_transform, esugar_do_transform}]).
-include_lib("eunit/include/eunit.hrl").


esugar_do_transform_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
            {<<"error_monad">>, fun error_monad/0}
        ]
    }.


error_monad() ->
    ?assertEqual({error, "First must be an integer!"}, safe_muldiv(2.4, 5)),
    ?assertEqual({error, "Second must be an integer!"}, safe_muldiv(60, 4.4)),
    ?assertEqual({error, "Second must not be zero!"}, safe_muldiv(60, 0)),
    ?assertEqual({ok, {300, 12}}, safe_muldiv(60, 5)).


safe_muldiv(First, Second) ->
    do@([esugar_do_transform_error ||
        case is_integer(First) of
        true -> return(next);
        false -> fail("First must be an integer!")
        end,
        case is_integer(Second) of
        true -> return(next);
        false -> fail("Second must be an integer!")
        end,
        Product = First * Second,
        case Second =/= 0 of
        true -> return(next);
        false -> fail("Second must not be zero!")
        end,
        Quotient = First div Second,
        return({Product, Quotient})
    ]).

