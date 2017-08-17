-module(esugar_do_transform).
-export([parse_transform/2]).


parse_transform(Forms, _Options) ->
    walk_form(Forms, []).


walk_form({call, Line, {atom, _, do@}, [{lc, _, Monad, Qs}]}, MonadStack) ->
    {call, Line, {'fun', Line, {clauses, [{clause, Line, [], []
                        , do_transform(Qs, [Monad | MonadStack])}]}}, []};
walk_form({call, Line, {atom, _, return} = Return, As}, [Monad | _] = MonadStack) ->
    {call, Line, {remote, Line, Monad, Return}, walk_form(As, MonadStack)};
walk_form({call, Line, {atom, _, fail} = Fail, As}, [Monad | _] = MonadStack) ->
    {call, Line, {remote, Line, Monad, Fail}, walk_form(As, MonadStack)};
walk_form(Form, MonadStack) when is_tuple(Form) ->
    erlang:list_to_tuple(walk_form(erlang:tuple_to_list(Form), MonadStack));
walk_form(Forms, MonadStack) when is_list(Forms) ->
    [walk_form(Form, MonadStack) || Form <- Forms];
walk_form(Form, _MonadStack) -> Form.


do_transform([E], MonadStack) -> [walk_form(E, MonadStack)];
do_transform([{match, _, _, _} = E | Es], MonadStack) ->
    [walk_form(E, MonadStack) | do_transform(Es, MonadStack)];
do_transform([{generate, Line, Pattern, E} | Es], [Monad | _] = MonadStack) ->
    [{call, Line, {remote, Line, Monad, {atom, Line, '>>='}}, [walk_form(E, MonadStack),
        {'fun', Line, {clauses, [{clause, Line, [Pattern], [], do_transform(Es, MonadStack)}]}}]}];
do_transform([E | Es], [Monad | _] = MonadStack) ->
    Line = erlang:element(2, E), Pattern = {var, Line, '_'},
    [{call, Line, {remote, Line, Monad, {atom, Line, '>>='}}, [walk_form(E, MonadStack),
        {'fun', Line, {clauses, [{clause, Line, [Pattern], [], do_transform(Es, MonadStack)}]}}]}].

