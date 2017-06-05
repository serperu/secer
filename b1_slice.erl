-module(b1). %2121

-export([numbers/2]).

numbers(A, _) ->
    C = fn(A, undef).

fn(X, _) ->
    if
        X > 5 ->
            X;
        true ->
            X + 2
    end.