-module(esugar_import_as_transform).
-export([format_error/1]).
-export([parse_transform/2]).


format_error({Format, Args}) ->
    lists:flatten(io_lib:format(Format, Args));
format_error(AnyThing) -> AnyThing.


parse_transform(Forms, _Options) ->
    walk_form(Forms).


walk_form([{attribute, Line, import_as, AttrValue} | T]) ->
    case check_import_as(AttrValue) of
    ok ->
        {Module, AsList} = AttrValue,
        lists:append(do_transform(Line, Module, AsList), walk_form(T));
    ErrArgument ->
        [{error, {Line, ?MODULE, ErrArgument}} | walk_form(T)]
    end;
walk_form([H | T]) -> [H | walk_form(T)]; walk_form([]) -> [].


do_transform(Line, Module, [{{F, Arity}, Alias} | T]) ->
    Vars = [{var, Line, erlang:list_to_atom("Var" ++ integer_to_list(I))}
                                                 || I <- lists:seq(1, Arity)],
    Body = {call, Line, {remote, Line, {atom, Line, Module}
                                                    , {atom, Line, F}}, Vars},
    Func = {function, Line, Alias, Arity, [{clause, Line, Vars, [], [Body]}]},
    [Func | do_transform(Line, Module, T)];
do_transform(_Line, _Module, []) -> [].


check_import_as(AttrValue) when not is_tuple(AttrValue); tuple_size(AttrValue) =/= 2 ->
    {"`AttrValue: ~p` is expected to be a tuple of size 2", [AttrValue]};
check_import_as({Module, _AsList}) when not is_atom(Module) ->
    {"`Module: ~p` is expected to be an atom", [Module]};
check_import_as({_Module, AsList}) when not is_list(AsList) ->
    {"`AsList: ~p` is expected to be a list", [AsList]};
check_import_as({_Module, [FArityAlias | _T]}) when not is_tuple(FArityAlias); tuple_size(FArityAlias) =/= 2 ->
    {"`FArityAlias: ~p` is expected to be a tuple of size 2", [FArityAlias]};
check_import_as({_Module, [{FArity, _Alias} | _T]}) when not is_tuple(FArity); tuple_size(FArity) =/= 2 ->
    {"`FArity: ~p` is expected to be a form of `F/Arity`", [FArity]};
check_import_as({_Module, [{_FArity, Alias} | _T]}) when not is_atom(Alias) ->
    {"`Alias: ~p` is expected to be an atom", [Alias]};
check_import_as({_Module, [{{F, _Arity}, _Alias} | _T]}) when not is_atom(F) ->
    {"`F: ~p` is expected to be an atom", [F]};
check_import_as({_Module, [{{_F, Arity}, _Alias} | _T]}) when not is_integer(Arity); Arity < 0; 255 < Arity ->
    {"`Arity: ~p` is expected to be a non_neg_integer no more than 255", [Arity]};
check_import_as({Module, [_H | T]}) -> check_import_as({Module, T});
check_import_as({_Module, []}) -> ok.

