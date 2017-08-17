# An Erlang Syntactic Sugar Library

## Motivation
I first encountered Erlang around 2014 and fell in love with this "micro operating system" that solved many problems in my work. But when talking with other people, I also heard some complaints about Erlang, most of them about its syntax. One day I discovered an open-source logging library called [Lager](https://github.com/erlang-lager/lager), which uses the compiler parameter `parse_transform` to transform source code in customizable ways. So I started learning more about `parse_transform` and wrote this project. On the one hand, it serves as a demo showing that we can do a lot of cool things with `parse_transform`; on the other hand, you can also think of it as an Erlang syntactic sugar library that can be used or extended.

## Pipe Operator
Many languages (such as F# and Elixir) support a pipeline operator, which passes the result of the previous expression as an argument to the next function call, forming a call chain. For example, if we want to implement an `md5` function in Erlang, the traditional way to write it might look like this:
```erlang
-spec md5(Data :: iodata()) -> Digest :: string().
md5(Data) ->
    _Digest = string:to_lower(lists:flatten(lists:map(fun(E) ->
        [integer_to_list(X, 16) || X <- [E div 16, E rem 16]]
    end, binary_to_list(erlang:md5(Data))))).
```
If you are tired of the nested calls above, this library allows the following pipeline style, which is equivalent to the traditional notation:
```erlang
-spec md5(Data :: iodata()) -> Digest :: string().
md5(Data) ->
    _Digest = pipe@(Data
        ! fun erlang:md5/1
        ! binary_to_list()
        ! lists:map(fun(E) -> [integer_to_list(X, 16)
                   || X <- [E div 16, E rem 16]] end)
        ! lists:flatten()
        ! fun string:to_lower/1).
```

## Do Block
One of the most annoying things about writing business logic in Erlang is the rocket-like nesting. For example, let's write a function that returns the product and quotient of two integers.
```erlang
muldiv(First, Second) ->
    case is_integer(First) of
        true ->
            case is_integer(Second) of
                true ->
                    Product = First * Second,
                    case Second =/= 0 of
                        true ->
                            Quotient = First div Second,
                            {ok, {Product, Quotient}};
                        false ->
                            {error, "Second must not be zero!"}
                    end;
                false ->
                    {error, "Second must be an integer!"}
            end;
        false ->
            {error, "First must be an integer!"}
    end.
```
Does it look like a rocket sprinting to the right? That is not an exaggeration. Real business logic may contain even more conditional branches, and it becomes even more painful when a change in requirements adds another one. One way to solve this problem is to split the business logic into smaller pieces, but that is not a cure-all you can use every time. Here we provide a Haskell-like `do` syntax that helps you avoid this annoying nesting.
```erlang
muldiv(First, Second) ->
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
```
To use the `do` syntax above, you only need to implement your own `monad` instance (here we implement `esugar_do_transform_error`).


## Import As
Many languages have the concept of modules, and each module contains a large number of functions. When another file wants to use those functions, they need to be imported into that file. Erlang does not usually work that way; instead, it uses the `M:F(...)` form to call an external function. Of course, it also supports the `-import(M, [F/A]).` syntax. Once you specify the module name, you can call `F(...)` directly. We now provide a more powerful import syntax that lets you rename imported functions, so you can call an external function with whatever name you like in the current module.
```erlang
-module(example).
-compile([{parse_transform, esugar_import_as_transform}]).
-import_as({lists, [{seq/2, range}, {reverse/1, rev}]}).
-export([run/0]).

run() ->
    [3, 2, 1] = rev(range(1, 3)).
```
However, I personally prefer the fully qualified calling style, and I rarely even use the built-in `import` syntax. This avoids many low-level mistakes, so the `import_as` syntax implemented here is not something I generally recommend.


## Bind Repeatedly
In Erlang, variables are actually immutable. When we bind `X` to the value `1` with `X = 1`, we cannot later change it with `X = 2`. The benefits of immutability are covered in almost every book that introduces functional programming, so I will not say much more here. So how do we "modify" a variable? Erlang's answer is: "You do not modify it. You create a new variable instead. Why do you want to modify it?" So Erlang encourages us to write code like this:
```erlang
norm(X, Y) ->
    T1 = 0,
    T2 = T1 + X * X,
    T3 = T2 + Y * Y,
    T4 = math:sqrt(T3),
    T4.
```
I am exaggerating the example a little here; you certainly do not need to create this many new variables in normal code. However, with repeated binding syntax, we can write it like this:
```erlang
norm(X, Y) ->
    T = 0,
    T = T + X * X,
    T = T + Y * Y,
    T = math:sqrt(T),
    T.
```
Doesn't this contradict Erlang's rule that already-bound variables cannot be modified? Not really. The code above is actually converted into the following intermediate form at compile time:
```erlang
norm(X@1, Y@1) ->
    T@1 = 0,
    T@2 = T@1 + X@1 * X@1,
    T@3 = T@2 + Y@1 * Y@1,
    T@4 = math:sqrt(T@3),
    T@4.
```
That is to say, every time a variable appears on the left side of a new match, even if it has the same name, it is converted into a new numbered variable. When the variable appears on the right side, the latest existing numbered version is used. One question remains: what if I want to use the variable I just bound in the pattern, instead of creating a new one? In that case, you can add `@` to the variable.
```erlang
norm(X, Y) ->
    T = 0,
    T = T + X * X,
    T = T + Y * Y,
    T@ = X * X + Y * Y,
    T = math:sqrt(T),
    T.
```
It is equivalent to the following:
```erlang
norm(X@1, Y@1) ->
    T@1 = 0,
    T@2 = T@1 + X@1 * X@1,
    T@3 = T@2 + Y@1 * Y@1,
    T@3 = X@1 * X@1 + Y@1 * Y@1,
    T@4 = math:sqrt(T@3),
    T@4.
```


## At The End
Finally, you may ask: what is the practical use of these features? Honestly, not that much. Nothing becomes impossible without them. As I said at the beginning, you can think of this project as a demo that shows how many interesting things we can do at the syntax level in Erlang. You can also think of it as a library to use or extend, but I cannot guarantee its stability. \_(:з」∠)\_
