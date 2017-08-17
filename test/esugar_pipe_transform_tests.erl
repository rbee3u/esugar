-module(esugar_pipe_transform_tests).
-compile([{parse_transform, esugar_pipe_transform}]).
-include_lib("eunit/include/eunit.hrl").


esugar_pipe_transform_test_() ->
    {foreach,
        fun() -> error_logger:tty(false) end,
        fun(_) -> error_logger:tty(true) end,
        [
              {<<"basic_operation">>, fun basic_operation/0}
        ]
    }.


basic_operation() ->
    ?assertEqual(pipe@(0), 0),
    ?assertEqual(pipe@(3.14), 3.14),
    ?assertEqual(pipe@(3.14 ! fun erlang:trunc/1), 3),
    ?assertEqual(pipe@(3 ! math:pow(2)), 8.0),
    ?assertEqual(pipe@(3.14 ! fun erlang:trunc/1 ! math:pow(2)), 8.0),
    ?assertEqual(pipe@(3 ! fun(X) -> X + 2 end), 5),
    Data = "foobar",
    ?assertEqual("3858f62230ac3c915f300c664312c63f", md5(Data)),
    ok.


-spec md5(Data :: iodata()) -> Digest :: string().
md5(Data) ->
    _Digest = pipe@(Data
        ! fun erlang:md5/1
        ! binary_to_list()
        ! lists:map(fun(E) -> [integer_to_list(X, 16)
                   || X <- [E div 16, E rem 16]] end)
        ! lists:flatten()
        ! fun string:to_lower/1).

