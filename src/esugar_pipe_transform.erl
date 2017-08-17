-module(esugar_pipe_transform).
-export([parse_transform/2]).


parse_transform(Forms, _Options) ->
    walk_form(Forms).


walk_form({call, Line, {atom, _, pipe@}, [{op, _, '!', First, Second}]}) ->
    do_transform(Line, First, Second);
walk_form({call, _Line, {atom, _, pipe@}, [First]}) ->
    First;
walk_form(Form) when is_tuple(Form) ->
    erlang:list_to_tuple(walk_form(erlang:tuple_to_list(Form)));
walk_form(Forms) when is_list(Forms) ->
    [walk_form(Form) || Form <- Forms];
walk_form(Form) -> Form.


do_transform(Line, First, {op, _, '!', {call, _, Func, Args}, Second}) ->
    do_transform(Line, {call, Line, Func, Args ++ [First]}, Second);
do_transform(Line, First, {op, _, '!', Func, Second}) ->
    do_transform(Line, {call, Line, Func, [First]}, Second);
do_transform(Line, First, {call, _, Func, Args}) ->
    {call, Line, Func, Args ++ [First]};
do_transform(Line, First, Func) ->
    {call, Line, Func, [First]}.

