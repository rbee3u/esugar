-module(esugar_util).
-export([abstract/2]).
-export([source/2]).


abstract(OFile, Module) when is_atom(Module) ->
    Path = code:which(Module),
    {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(Path, [abstract_code]),
    file:write_file(OFile, io_lib:format("~p~n", [AC]));
abstract(OFile, IFile) when is_list(IFile) ->
    {ok, AC} = epp:parse_file(IFile, []),
    file:write_file(OFile, io_lib:format("~p~n", [AC])).


source(OFile, Module) when is_atom(Module) ->
    Path = code:which(Module),
    {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(Path, [abstract_code]),
    file:write_file(OFile, erl_prettypr:format(erl_syntax:form_list(AC))).

