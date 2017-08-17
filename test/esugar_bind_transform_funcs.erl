-module(esugar_bind_transform_funcs).
-compile([{parse_transform, esugar_bind_transform}]).
-export([f1/1, f2/2, f3/2, f4/2, f5/1, f6/1]).
-export([f7/0, f8/0]).
-export([f9/0, f10/0, f11/0, f12/0]).
-export([f13/1, f14/1, f15/1]).
-export([f16/1, f17/1]).


f1(_A) -> ok.
f2(_A, _B) -> ok.
f3(A, A) -> ok.

f4(0, _B) -> 0;
f4(A, A) -> 1;
f4(_A, _B) -> 2.

f5(A) when A > 0 -> 1;
f5(A) when A < 0 -> -1;
f5(_A) -> 0.

f6({A, B = #{x := A}}) -> B;
f6({_A, _B = #{x := C}}) -> C;
f6(_) -> ok.

f7() ->
    A = 1,
    A = A + A,
    B = 1,
    B = B + 1,
    {A, B}.

f8() ->
    A = 1,
    #{x := A} = #{x => A + 1},
    #{A@ := B} = #{2 => 2},
    {A, B}.

f9() ->
    A = 1,
    F = fun() -> A end,
    F().

f10() ->
    A = 1,
    F = fun() -> A = A + 1, A end,
    {A, F()}.

f11() ->
    A = 1,
    F = fun(A) -> A = A + 1, A end,
    {A, F(2)}.

f12() ->
    A = 1,
    F = fun Add(0) -> 0;
        Add(A) -> 1 + Add(A - 1)
    end,
    {A, F(2)}.

f13(A) ->
    if A > 0 ->
        A = A - 1;
    true ->
        A = A
    end,
    A.

f14(A) ->
    case {1, 3} of
    {A@, _B} ->
        {1, A};
    {A, 2} ->
        {2, A};
    {A, _B} ->
        {3, A}
    end.

f15(A) ->
    try erlang:integer_to_list(A) of
    [A, _B] ->
        case A of
        $1 ->
            -1;
        _ ->
            A
        end;
    [A] ->
        self() ! {go, A},
        receive
        A ->
            A
        end;
    _ ->
        undef
    catch A:B ->
        {A, B}
    end.

f16(X) ->
    [E || E <- lists:seq(1, 5), E =/= X].

f17(X) ->
    Y = 4,
    {Y, [Y || Y <- lists:seq(1, 5), Y =/= X]}.

