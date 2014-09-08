-module(sexpr).

-export([is_sexpr/1, test/0]).


is_sexpr([]) ->
    true;
is_sexpr([Head|Tail]) ->
    case is_sexpr(Head) of
        false ->
            false;
        true ->
            is_sexpr(Tail)
    end;
is_sexpr(Expr) ->
    is_atom(Expr).


test() ->
    true = is_sexpr(a),
    false = is_sexpr(1),
    true = is_sexpr([]),
    true = is_sexpr([a]),
    true = is_sexpr([a, b, c]),
    false = is_sexpr([1]),
    false = is_sexpr([a, 1]),
    ok.
