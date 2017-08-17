-module(esugar_do_transform_error).
-export(['>>='/2, return/1, fail/1]).
-export_type([error/1]).


-type error(A) :: {ok, A} | {error, any()}.


-spec '>>='(MA, fun((A) -> MB)) -> MB when
            MA :: error(A), MB :: error(_B).
'>>='({ok, Result}, Fun) -> Fun(Result);
'>>='({error, _} = Error, _Fun) -> Error.


-spec return(A) -> error(A).
return(X) -> {ok, X}.


-spec fail(any()) -> error(_A).
fail(X) -> {error, X}.

