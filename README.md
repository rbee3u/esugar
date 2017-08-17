# An Erlang Syntactic Sugar Library

## Motivation
I probably met Erlang in 2014 and fell in love with this "micro operating system" that solved many of my work problems. But in communicating with others, I also heard some complaints about Erlang, the most of which is about its grammar! Until one day I found an open source log library named [Lager](https://github.com/erlang-lager/lager), there is a trick to use the compiler parameter `parse_transform` to convert the source code to your own wishes. So I went to check out the relevant information of `parse_transform` and wrote this project: on the one hand, it shows as demo that we can do a lot of cool things with `parse_transform`; on the other hand, you can also think of it as an Erlang syntactic sugar library is used or extended.

## Pipe Operator
Many languages (such as F# and Elixir) support the pipeline operator, which passes the result of the previous expression as a parameter to the next function (call), forming a call chain. For example, if we want to implement an `md5` function with Erlang, the traditional way of writing might be like this:
```erlang
-spec md5(Data :: iodata()) -> Digest :: string().
md5(Data) ->
    _Digest = string:to_lower(lists:flatten(lists:map(fun(E) ->
        [integer_to_list(X, 16) || X <- [E div 16, E rem 16]]
    end, binary_to_list(erlang:md5(Data))))).
```
If you are tired of the nested call above, then we will allow the following pipelining, which is equivalent to the previous traditional notation:
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
The most annoying thing about writing business logic with Erlang is the rocket-like nesting. For example, let's write a function that returns the result of multiplying and dividing two integers.
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
Does it look like a rocket that sprints to the right? This is not an exaggeration. There may be more judgment branches in the real business logic, and it is even more distressing when a change in requirements requires a new judgment. One way to solve this problem is to split business logic reasonably, but this is not a panacea that can be used at any time. Here we provide a `do` syntax like Haskell that allows you to avoid this annoying nesting.
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
To use the `do` syntax like above, you only need to implement your own `monad` instance (here we implement `esugar_do_transform_error`).


## Import As
We know that many languages have the concept of modules, and a module contains a large number of functions. When other files want to use these functions, they need to be imported into the file. Erlang does not import but uses the `M:F(...)` method to call the external function. Of course, it also supports the syntax of `-import(M, [F/A]).` Specify the module name and you can call `F(...)` directly. We now provide a more powerful import syntax that renames the imported function so that you can call the external function with the name you like in the current module.
```erlang
-module(example).
-compile([{parse_transform, esugar_import_as_transform}]).
-import_as({lists, [{seq/2, range}, {reverse/1, rev}]}).
-export([run/0]).

run() ->
    [3, 2, 1] = rev(range(1, 3)).
```
However, I prefer the most primitive method of calling, even the `import` syntax is rarely used. This avoids many low-level errors, so the `import_as` syntax we implemented is not recommended.


## Bind Repeatedly
In Erlang, the variable is actually immutable. When we bind `X` to the value `1` with `X = 1`, we can't say no later and let `X = 2`. The benefits of this immutable property are bound to be written in any book that introduces functional programming, and I won't say much here. So how do we modify a variable, Erlang says: "You can't modify it at any time, but you can create a new variable, why do you want to modify it?" So Erlang encourages us to write code as follow:
```erlang
norm(X, Y) ->
    T1 = 0,
    T2 = T1 + X * X,
    T3 = T2 + Y * Y,
    T4 = math:sqrt(T3),
    T4.
```
I am just exaggerating the example for example, you certainly don't have to create so many new variables. However, after using the repeated binding syntax, we can actually write this:
```erlang
norm(X, Y) ->
    T = 0,
    T = T + X * X,
    T = T + Y * Y,
    T = math:sqrt(T),
    T.
```
Doesn't it mean that Erlang doesn't let us modify the variables that are already bound, and it does. The above code will actually be converted to the following transition form at compile time:
```erlang
norm(X@1, Y@1) ->
    T@1 = 0,
    T@2 = T@1 + X@1 * X@1,
    T@3 = T@2 + Y@1 * Y@1,
    T@4 = math:sqrt(T@3),
    T@4.
```
That is to say, in each new matching mode as an left value, even if the same variable name, the number is converted into a new variable. As the variable name of the right value, the largest number that already exists is used. Then there is one problem left. If I want to use the variable that I just bound in the pattern instead of re-creating it, you can add `@` to the variable at this time.
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
Finally, maybe you will ask, what is the use of these things? I will tell you that it doesn't make much sense. Nothing is impossible because of the lack of them, as I said at the beginning: You can think of it as a demo, showing that we can do a lot of interesting things in the grammatical level of Erlang; you can also think of it as a library to use or extend, but I can't guarantee its stability. \_(:з」∠)\_

