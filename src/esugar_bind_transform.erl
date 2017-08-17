-module(esugar_bind_transform).
-export([format_error/1]).
-export([parse_transform/2]).

-define(get_scope_stack(), erlang:get(scope_stack)).
-define(set_scope_stack(L), erlang:put(scope_stack, L)).
-define(del_scope_stack(), erlang:erase(scope_stack)).
-define(add_scope(), ?set_scope_stack([#{} | ?get_scope_stack()])).
-define(del_scope(), ?set_scope_stack(erlang:tl(?get_scope_stack()))).
-define(get_scope(), erlang:hd(?get_scope_stack())).
-define(set_scope(S), ?set_scope_stack([S | tl(?get_scope_stack())])).
-define(get_pattern(), erlang:get(pattern)).
-define(set_pattern(P), erlang:put(pattern, P)).
-define(del_pattern(), erlang:erase(pattern)).
-define(clr_pattern(), begin ?set_scope(maps:merge(?get_scope()
                        , ?get_pattern())), ?del_pattern() end).


format_error({Format, Args}) ->
    lists:flatten(io_lib:format(Format, Args));
format_error(AnyThing) -> AnyThing.


parse_transform(Forms, _Options) ->
    walk_form(Forms).


walk_form({function, _Line, _Name, _Arity, _Clauses} = Function) ->
    try walk_function(Function) catch
    throw:{ErrLine, ErrArgument} ->
        ?del_scope_stack(), ?del_pattern(),
        {error, {ErrLine, ?MODULE, ErrArgument}};
    ErrType:ErrReason:StackTrace ->
        erlang:raise(ErrType, ErrReason, StackTrace)
    end;
walk_form(Forms) when is_list(Forms) ->
    [walk_form(Form) || Form <- Forms];
walk_form(Form) -> Form.


walk_function({function, Line, Name, Arity, Clauses}) ->
    ?set_scope_stack([]),
    Clauses2 = walk_function_clauses(Clauses),
    ?del_scope_stack(),
    {function, Line, Name, Arity, Clauses2}.


walk_function_clauses([Clause | T]) ->
    ?add_scope(),
    Clause2 = walk_clause(Clause),
    ?del_scope(),
    [Clause2 | walk_function_clauses(T)];
walk_function_clauses([]) -> [].


walk_icrtry_clauses(Clauses, InitScope, BaseScope) ->
    lists:mapfoldl(fun(Clause, Acc) ->
        ?set_scope(BaseScope),
        Clause2 = walk_clause(Clause),
        Acc2 = merge_scope(Acc, ?get_scope()),
        {Clause2, Acc2}
    end, InitScope, Clauses).


merge_scope(ScopeA, ScopeB) ->
    maps:fold(fun(K, V, Map) -> case Map of
        #{K := U} when U > V -> Map; _ -> Map#{K => V}
    end end, ScopeA, ScopeB).


walk_clause({clause, Line, Head, Guard, Body}) ->
    ?set_pattern(#{}),
    Head2 = walk_pattern(Head),
    ?clr_pattern(),
    Guard2 = walk_gexpr(Guard),
    Body2 = walk_expr(Body),
    {clause, Line, Head2, Guard2, Body2}.


walk_pattern({var, Line, Var}) ->
    {var, Line, left_var(Line, Var)};
walk_pattern(Pattern) when is_tuple(Pattern) ->
    erlang:list_to_tuple(walk_pattern(erlang:tuple_to_list(Pattern)));
walk_pattern(Patterns) when is_list(Patterns) ->
    [walk_pattern(Pattern) || Pattern <- Patterns];
walk_pattern(Pattern) -> Pattern.


walk_gexpr({var, Line, Var}) ->
    {var, Line, right_var(Line, Var)};
walk_gexpr(GExpr) when is_tuple(GExpr) ->
    erlang:list_to_tuple(walk_gexpr(erlang:tuple_to_list(GExpr)));
walk_gexpr(GExprs) when is_list(GExprs) ->
    [walk_gexpr(GExpr) || GExpr <- GExprs];
walk_gexpr(GExpr) -> GExpr.


walk_expr({var, Line, Var}) ->
    {var, Line, right_var(Line, Var)};
walk_expr({match, Line, Pattern, Expr}) ->
    Expr2 = walk_expr(Expr),
    ?set_pattern(#{}),
    Pattern2 = walk_pattern(Pattern),
    ?clr_pattern(),
    {match, Line, Pattern2, Expr2};
walk_expr({'if', Line, Clauses}) ->
    Scope = ?get_scope(),
    {Clauses2, Scope2} = walk_icrtry_clauses(Clauses, Scope, Scope),
    ?set_scope(Scope2),
    {'if', Line, Clauses2};
walk_expr({'case', Line, Expr, Clauses}) ->
    Expr2 = walk_expr(Expr),
    Scope = ?get_scope(),
    {Clauses2, Scope2} = walk_icrtry_clauses(Clauses, Scope, Scope),
    ?set_scope(Scope2),
    {'case', Line, Expr2, Clauses2};
walk_expr({'receive', Line, Clauses}) ->
    Scope = ?get_scope(),
    {Clauses2, Scope2} = walk_icrtry_clauses(Clauses, Scope, Scope),
    ?set_scope(Scope2),
    {'receive', Line, Clauses2};
walk_expr({'receive', Line, Clauses, Timeout, TimeoutExpr}) ->
    Scope = ?get_scope(),
    {Clauses2, Scope2} = walk_icrtry_clauses(Clauses, Scope, Scope),
    ?set_scope(Scope),
    {Timeout2, TimeoutExpr2} = walk_expr({Timeout, TimeoutExpr}),
    Scope3 = merge_scope(Scope2, ?get_scope()),
    ?set_scope(Scope3),
    {'receive', Line, Clauses2, Timeout2, TimeoutExpr2};
walk_expr({'try', Line, Expr, CaseClauses, CatchClauses, AfterExpr}) ->
    Expr2 = walk_expr(Expr),
    Scope = ?get_scope(),
    {CaseClauses2, Scope2} = walk_icrtry_clauses(CaseClauses, Scope, Scope),
    {CatchClauses2, Scope3} = walk_icrtry_clauses(CatchClauses, Scope2, Scope),
    ?set_scope(Scope),
    AfterExpr2 = walk_expr(AfterExpr),
    Scope4 = merge_scope(Scope3, ?get_scope()),
    ?set_scope(Scope4),
    {'try', Line, Expr2, CaseClauses2, CatchClauses2, AfterExpr2};
walk_expr({'fun', Line, Body}) ->
    case Body of
    {clauses, Clauses} ->
        Clauses2 = walk_function_clauses(Clauses),
        {'fun', Line, {clauses, Clauses2}};
    _ ->
        {'fun', Line, walk_expr(Body)}
    end;
walk_expr({named_fun, Line, Name, Clauses}) ->
    ?add_scope(),
    ?set_pattern(#{}),
    Name2 = left_var(Line, Name),
    ?clr_pattern(),
    Clauses2 = walk_function_clauses(Clauses),
    ?del_scope(),
    {named_fun, Line, Name2, Clauses2};
walk_expr({Tag, Line, Expr, Qs}) when Tag =:= lc; Tag =:= bc ->
    ?add_scope(),
    Qs2 = lc_bc_quals(Qs),
    Expr2 = walk_expr(Expr),
    ?del_scope(),
    {Tag, Line ,Expr2, Qs2};
walk_expr(Expr) when is_tuple(Expr) ->
    erlang:list_to_tuple(walk_expr(erlang:tuple_to_list(Expr)));
walk_expr(Exprs) when is_list(Exprs) ->
    [walk_expr(Expr) || Expr <- Exprs];
walk_expr(Expr) -> Expr.


lc_bc_quals([{Tag, Line, Pattern, Expr} | T]) when
            Tag =:= generate; Tag =:= b_generate ->
    Expr2 = walk_expr(Expr),
    ?set_pattern(#{}),
    Pattern2 = walk_pattern(Pattern),
    ?clr_pattern(),
    [{Tag, Line, Pattern2, Expr2} | lc_bc_quals(T)];
lc_bc_quals([Expr | T]) ->
    Expr2 = walk_expr(Expr),
    [Expr2 | lc_bc_quals(T)];
lc_bc_quals([]) -> [].


concat(Var, No) ->
    erlang:list_to_atom(
           erlang:atom_to_list(Var)
        ++ "@"
        ++ erlang:integer_to_list(No)
    ).

left_var(_Line, '_') ->
    '_';
left_var(Line, VarOrVarAt) ->
    case lists:reverse(erlang:atom_to_list(VarOrVarAt)) of
    "@" ++ RevInit ->
        Var = erlang:list_to_atom(lists:reverse(RevInit)),
        LastCharIsAt = true;
    _ ->
        Var = VarOrVarAt,
        LastCharIsAt = false
    end,
    Pattern = ?get_pattern(),
    case Pattern of
    #{Var := No} ->
        concat(Var, No);
    _ ->
        ScopeStack = ?get_scope_stack(),
        case left_no(Var, ScopeStack) of
        No when is_integer(No) ->
            case LastCharIsAt of
            false ->
                No2 = No + 1;
            _ ->
                No2 = No
            end,
            ?set_pattern(Pattern#{Var => No2}),
            concat(Var, No2);
        _ ->
            case LastCharIsAt of
            false ->
                No = 1,
                ?set_pattern(Pattern#{Var => No}),
                concat(Var, No);
            _ ->
                erlang:throw({Line, {"variable ~p is unbound", [Var]}})
            end
        end
    end.

left_no(Var, [Scope | T]) ->
    case Scope of
    #{Var := No} -> No;
    _ -> left_no(Var, T)
    end;
left_no(_Var, _ScopeStack) ->
    undefined.


right_var(_Line, '_') ->
    '_';
right_var(Line, Var) ->
    ScopeStack = ?get_scope_stack(),
    case right_no(Var, ScopeStack) of
    No when is_integer(No) ->
        concat(Var, No);
    _ -> erlang:throw({Line, {"variable ~p is unbound", [Var]}})
    end.

right_no(Var, [Scope | T]) ->
    case Scope of
    #{Var := No} -> No;
    _ -> right_no(Var, T)
    end;
right_no(_Var, _ScopeStack) ->
    undefined.

