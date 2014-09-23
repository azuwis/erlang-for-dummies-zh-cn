-module(function).
-compile({no_auto_import, [apply/2]}).

-export([test/0, new_env/0, eval/2]).


subst(_, []) ->
    none;
subst(K, [{K, V}|_]) ->
    {ok, V};
subst(K, [_|T]) ->
    subst(K, T).


append([], L2) ->
    L2;
append([H|T], L2) ->
    [H|append(T, L2)].


zip([], []) ->
    [];
zip([H1|T1], [H2|T2]) ->
    [{H1,H2}|zip(T1, T2)].


last([E]) ->
    E;
last([_|T]) ->
    last(T).


eq([], []) ->
    true;
eq(X, Y)
  when is_atom(X), is_atom(Y) ->
    X =:= Y;
eq(_, _) ->
    false.


call({fn, quote}, [X], Env) ->
    {{data, X}, Env};
call({fn, atom}, Expr, Env) ->
    {[{data, X}], Env1} = eval_list(Expr, Env),
    {{data, is_atom(X)}, Env1};
call({fn, eq}, Expr, Env) ->
    {[{data, X}, {data, Y}], Env1} = eval_list(Expr, Env),
    {{data, eq(X,Y)}, Env1};
call({fn, car}, Expr, Env) ->
    {[{data, [H|_]}], Env1} = eval_list(Expr, Env),
    {{data, H}, Env1};
call({fn, cdr}, Expr, Env) ->
    {[{data, [_|T]}], Env1} = eval_list(Expr, Env),
    {{data, T}, Env1};
call({fn, cons}, Expr, Env) ->
    {[{data, X}, {data, Y}], Env1} = eval_list(Expr, Env),
    {{data, [X|Y]}, Env1};
call({fn, 'cond'}, [[P,E]|T], Env) ->
    {{data, R}, Env1} = apply(P, Env),
    case R of
        false ->
            call({fn, 'cond'}, T, Env1);
        true ->
            apply(E, Env1)
    end;
call({fn, label}, [X,Y], Env) ->
    {Y1, Env1} = apply(Y, Env),
    {Y1, [{X, Y1}|Env1]};
call({fn, lambda}, [P,E], Env) ->
    {{lambda, {P,E}}, Env};
call({lambda, {P,E}}, Args, Env) ->
    {Args1, Env1} = eval_list(Args, Env),
    {V, _} = apply(E, append(zip(P, Args1), Env1)),
    {V, Env1}.


apply(Expr, Env)
  when is_atom(Expr) ->
    {ok, Value} = subst(Expr, Env),
    {Value, Env};
apply([H|T], Env) ->
    {Fun, Env1} = apply(H, Env),
    call(Fun, T, Env1).


eval_list([], Env) ->
    {[], Env};
eval_list([H|T], Env) ->
    {VH, Env1} = apply(H, Env),
    {VT, Env2} = eval_list(T, Env1),
    {[VH|VT], Env2}.


new_env() ->
    [{quote,{fn,quote}},
     {atom,{fn,atom}},
     {eq,{fn,eq}},
     {car,{fn,car}},
     {cdr,{fn,cdr}},
     {cons,{fn,cons}},
     {'cond',{fn,'cond'}},
     {lambda,{fn,lambda}},
     {label,{fn,label}}].


eval(List, Env) ->
    {Values, Env1} = eval_list(List, Env),
    {last(Values), Env1}.


eval(List) ->
    {V, _} = eval(List, new_env()),
    V.


test(subst) ->
    none = subst(a, []),
    {ok, c} = subst(a,[{a,c}]),
    {ok, c} = subst(a,[{b,d},{a,c}]),
    none = subst(a,[{b,d}]),
    {ok, c} = subst(a,[{a,c},{b,d},{a,e}]);
test(quote) ->
    {data, a} = eval([[quote,a]]),
    {data, [a,b,c]} = eval([[quote,[a,b,c]]]);
test(atom) ->
    {data, true} = eval([[atom,[quote,a]]]),
    {data, false} = eval([[atom,[quote,[a,b,c]]]]),
    {data, false} = eval([[atom,[quote,[]]]]);
test(eq) ->
    {data, true} = eval([[eq,[quote,a],[quote,a]]]),
    {data, false} = eval([[eq,[quote,a],[quote,b]]]),
    {data, true} = eval([[eq,[quote,[]],[quote,[]]]]),
    {data, false} = eval([[eq,[quote,[a,b]],[quote,[a,b]]]]);
test(list) ->
    {data, a} = eval([[car,[quote,[a,b,c]]]]),
    {data, [b,c]} = eval([[cdr,[quote,[a,b,c]]]]),
    {data, [a,b,c]} = eval([[cons,[quote,a],[quote,[b,c]]]]);
test('cond') ->
    {data, second} =
        eval(
          [['cond',
            [[eq,[quote,a],[quote,b]],
             [quote,first]],
            [[atom,[quote,a]],
             [quote,second]]]]);
test(label) ->
    {data, a} =
        eval(
          [[label, a, [quote, a]],
           a]),
    {data, [a,b,c]} =
        eval(
          [[label, a, [quote, [a,b,c]]],
           a]);
test(lambda) ->
    {data, [a,b]} =
        eval(
          [[[lambda,[x],
             [cons,x,[quote,[b]]]],
            [quote,a]]]),
    {data, [z,b,c]} =
        eval(
          [[[lambda,[x,y],
             [cons,x,[cdr,y]]],
            [quote,z],
            [quote,[a,b,c]]]]),
    {data, [a,b,c]} =
        eval(
          [[[lambda,[f],
             [f,[quote,[b,c]]]],
            [lambda,[x],
             [cons,[quote,a],x]]]]).


test() ->
    test(subst),
    test(quote),
    test(atom),
    test(eq),
    test(list),
    test('cond'),
    test(lambda),
    test(label),
    ok.
