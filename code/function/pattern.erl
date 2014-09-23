-module(pattern).

-import(function, [new_env/0, eval/2]).
-export([test/0]).


init_env() ->
    {_, Env} =
        eval(
          [[label, 'not',
            [lambda, [x],
             ['cond',
              [x,
               [quote, false]],
              [[quote, true],
               [quote, true]]
             ]
            ]
           ],

           [label, 'and',
            [lambda, [x,y],
             ['cond',
              [x,
               ['cond',
                [y,
                 [quote, true]],
                [[quote, true],
                 [quote, false]]
               ]],
              [[quote, true],
               [quote, false]]
             ]
            ]
           ],

           [label, last,
            [lambda, [x],
             ['cond',
              [[eq, [cdr, x], [quote, []]],
               [car, x]],
              [[quote, true],
               [last, [cdr, x]]]
             ]
            ]
           ],

           [label, member,
            [lambda, [x, l],
             ['cond',
              [[eq, l, [quote, []]],
               [quote, false]
              ],
              [[eq, x, [car, l]],
               [quote, true]],
              [[quote, true],
               [member, x, [cdr, l]]]
             ]
            ]
           ],

           [label, append,
            [lambda, [l1, l2],
             ['cond',

              [[eq, l1, [quote, []]],
               l2
              ],

              [[quote, true],
               [cons,
                [car, l1],
                [append, [cdr, l1], l2]
               ]
              ]

             ]
            ]
           ],

           [label, zip,
            [lambda, [l1, l2],
             ['cond',
              [['and',
                [eq, l1, [quote, []]],
                [eq, l2, [quote, []]]
               ],
               [quote, []]
              ],

              [[quote, true],
               [cons,
                [cons, [car, l1], [car, l2]],
                [zip, [cdr, l1], [cdr, l2]]
               ]
              ]

             ]
            ]
           ],

           [label, list1,
            [lambda, [x],
             [cons, x, [quote, []]]
            ]
           ],

           [label, list2,
            [lambda, [x, y],
             [cons, x, [list1, y]]
            ]
           ],

           [label, list3,
            [lambda, [x, y, z],
             [cons, x, [list2, y, z]]
            ]
           ],

           [label, subst,
            [lambda, [k,s],
             ['cond',
              [[eq, s, [quote, []]],
               [quote, none]],
              [[eq, k, [car, [car, s]]],
               [cons, [quote, ok], [cdr, [car, s]]]],
              [[quote, true],
               [subst, k, [cdr, s]]
              ]
             ]
            ]
           ],

           [label, equal,
            [lambda, [a,b],
             ['cond',
              [[eq, a, b],
               [quote, true]
              ],

              [['and',
                ['and', ['not', [atom, a]], ['not', [atom, b]]],
                ['and', ['not', [eq, a, [quote, []]]], ['not', [eq, b, [quote, []]]]]],
               ['and',
                [equal, [car, a], [car, b]],
                [equal, [cdr, a], [cdr, b]]]
              ],

              [[quote, true],
               [quote, false]
              ]
             ]
            ]
           ],

           [label, make_pattern,
            [lambda, [expr, vars],
             ['cond',
              [[atom, expr],
               ['cond',
                [[eq, expr, [quote, ignore]],
                 [quote, ignore]],
                [[member, expr, vars],
                 [cons, [quote, var], expr]],
                [[quote, true],
                 [cons, [quote, atom], expr]]
               ]
              ],

              [[eq, expr, [quote, []]],
               [quote, nil]],

              [[quote, true],
               [cons,
                [quote, cons],
                [cons,
                 [make_pattern, [car, expr], vars],
                 [make_pattern, [cdr, expr], vars]
                ]
               ]
              ]

             ]
            ]
           ],

           [label, match,
            [lambda, [value, pattern, table],
             ['cond',
              [[eq, table, [quote, false]],
               [quote, false]],
              [[eq, pattern, [quote, ignore]],
               table],
              [[eq, pattern, [quote, nil]],
               ['cond',
                [[eq, value, [quote, []]],
                 table
                ],
                [[quote, true],
                 [quote, false]
                ]
               ]
              ],
              [[eq, [car, pattern], [quote, atom]],
               ['cond',
                [[eq, value, [cdr, pattern]],
                 table],
                [[quote, true],
                 [quote, false]
                ]
               ]
              ],
              [[eq, [car, pattern], [quote, var]],
               [last,
                [list2,
                 [label, var, [subst, [cdr, pattern], table]],
                 ['cond',
                  [[eq, var, [quote, none]],
                   [cons, [cons, [cdr, pattern], value], table]
                  ],
                  [[eq, [car, var], [quote, ok]],
                   ['cond',
                    [[eq, [cdr, var], value],
                     table],
                    [[quote, true],
                     [quote, false]]
                   ]
                  ]
                 ]
                ]
               ]
              ],
              [[eq, [car, pattern], [quote, cons]],
               [match,
                [cdr, value],
                [cdr, [cdr, pattern]],
                [match, [car, value], [car, [cdr, pattern]], table]
               ]
              ]
             ]
            ]
           ],

           [label, apply_pattern,
            [lambda, [pattern, table],
             ['cond',
              [[eq, pattern, [quote, nil]],
               [quote, []]
              ],

              [[eq, [car, pattern], [quote, atom]],
               [cdr, pattern]
              ],

              [[eq, [car, pattern], [quote, var]],
               [cdr, [subst, [cdr, pattern], table]]
              ],

              [[eq, [car, pattern], [quote, cons]],
               [cons,
                [apply_pattern, [car, [cdr, pattern]], table],
                [apply_pattern, [cdr, [cdr, pattern]], table]
               ]
              ]

             ]
            ]
           ],

           [label, with_match,
            [lambda, [value, pattern, initial, fn],
             [last,
              [list2,

               [label, table,
                [match, value, pattern, initial]
               ],

               ['cond',
                [['not', [eq, table, [quote, false]]],
                 [fn, table]
                ]
               ]

              ]
             ]
            ]
           ],

           [label, apply,
            [lambda, [expr, env],
             ['cond',

              [[atom, expr],
               [cons,
                [cdr, [subst, expr, env]],
                env]
              ],

              [[quote, true],
               [last,
                [list2,
                 [label, result, [apply, [car, expr], env]],
                 [call, [car, result], [cdr, expr], [cdr, result]]
                ]
               ]
              ]

             ]
            ]
           ],

           [label, eval_list,
            [lambda, [list, env],
             ['cond',

              [[eq, list, [quote, []]],
               [cons, [quote, []], env]],

              [[quote, true],
               [last,
                [list3,
                 [label, v1, [apply, [car, list], env]],
                 [label, v2, [eval_list, [cdr, list], [cdr, v1]]],
                 [cons,
                  [cons, [car, v1], [car, v2]],
                  [cdr, v2]]
                ]
               ]
              ]

             ]
            ]
           ],

           [label, init_table,
            [lambda, [f, expr, env],
             [list3,
              [cons, [quote, f], f],
              [cons, [quote, expr], expr],
              [cons, [quote, env], env]
             ]
            ]
           ],

           [label, call,
            [lambda, [fn, expr, env],
             []
            ]
           ],

           [label, call,
            [lambda, [fn, expr, env],
             ['cond',

              [[equal, fn, [quote, [fn|quote]]],
               [with_match,
                expr,
                [make_pattern, [quote, [x]], [quote, [x]]],
                [init_table, fn, expr, env],
                [lambda, [table],
                 [apply_pattern,
                  [make_pattern, [quote, [[data|x]|env]], [quote, [x, env]]],
                  table]
                ]
               ]
              ],

              [[equal, fn, [quote, [fn|atom]]],
               [with_match,
                [eval_list, expr, env],
                [make_pattern, [quote, [[[data|x]]|env1]], [quote, [x, env1]]],
                [init_table, fn, expr, env],
                [lambda, [table],
                 [cons,
                  [cons,
                   [quote, data],
                   [atom, [cdr, [subst, [quote, x], table]]]
                  ],
                  [cdr, [subst, [quote, env1], table]]
                 ]
                ]
               ]
              ],

              [[equal, fn, [quote, [fn|eq]]],
               [with_match,
                [eval_list, expr, env],
                [make_pattern, [quote, [[[data|x], [data|y]]|env1]], [quote, [x, y, env1]]],
                [init_table, fn, expr, env],
                [lambda, [table],
                 [cons,
                  [cons,
                   [quote, data],
                   [eq,
                    [cdr, [subst, [quote, x], table]],
                    [cdr, [subst, [quote, y], table]]]
                  ],
                  [cdr, [subst, [quote, env1], table]]
                 ]
                ]
               ]
              ],

              [[equal, fn, [quote, [fn|car]]],
               [with_match,
                [eval_list, expr, env],
                [make_pattern, [quote, [[[data|[h|ignore]]]|env1]], [quote, [h, env1]]],
                [init_table, fn, expr, env],
                [lambda, [table],
                 [apply_pattern,
                  [make_pattern, [quote, [[data|h]|env]], [quote, [h, env]]],
                  table]
                ]
               ]
              ],

              [[equal, fn, [quote, [fn|cdr]]],
               [with_match,
                [eval_list, expr, env],
                [make_pattern, [quote, [[[data|[ignore|t]]]|env1]], [quote, [t, env1]]],
                [init_table, fn, expr, env],
                [lambda, [table],
                 [apply_pattern,
                  [make_pattern, [quote, [[data|t]|env]], [quote, [t, env]]],
                  table]
                ]
               ]
              ],

              [[equal, fn, [quote, [fn|cons]]],
               [with_match,
                [eval_list, expr, env],
                [make_pattern, [quote, [[[data|x], [data|y]]|env1]], [quote, [x, y, env1]]],
                [init_table, fn, expr, env],
                [lambda, [table],
                 [apply_pattern,
                  [make_pattern, [quote, [[data|[x|y]]|env]], [quote, [x, y, env]]],
                  table]
                ]
               ]
              ],

              [[equal, fn, [quote, [fn|'cond']]],
               [with_match,
                expr,
                [make_pattern, [quote, [[p,e]|t]], [quote, [p,e,t]]],
                [init_table, fn, expr, env],

                [lambda, [table],
                 [with_match,
                  [apply,
                   [cdr, [subst, [quote, p], table]],
                   env],
                  [make_pattern, [quote, [[data|r]|env1]], [quote, [r, env1]]],
                  table,

                  [lambda, [table],
                   ['cond',
                    [[eq, [cdr, [subst, [quote, r], table]], [quote, false]],
                     [call,
                      [quote, [fn|'cond']],
                      [cdr, [subst, [quote, t], table]],
                      [cdr, [subst, [quote, env1], table]]
                     ]
                    ],

                    [[eq, [cdr, [subst, [quote, r], table]], [quote, true]],
                     [apply,
                      [cdr, [subst, [quote, e], table]],
                      [cdr, [subst, [quote, env1], table]]
                     ]
                    ]

                   ]
                  ]

                 ]
                ]

               ]
              ],

              [[equal, fn, [quote, [fn|label]]],
               [with_match,
                expr,
                [make_pattern, [quote, [x,y]], [quote, [x,y]]],
                [init_table, fn, expr, env],

                [lambda, [table],
                 [with_match,
                  [apply, [cdr, [subst, [quote, y], table]], env],
                  [make_pattern, [quote, [y1|env1]], [quote, [y1, env1]]],
                  table,
                  [lambda, [table],
                   [apply_pattern,
                    [make_pattern, [quote, [y1|[[x|y1]|env1]]], [quote, [x,y1,env1]]],
                    table]
                  ]
                 ]
                ]
               ]
              ],

              [[equal, fn, [quote, [fn|lambda]]],
               [with_match,
                expr,
                [make_pattern, [quote, [p,e]], [quote, [p,e]]],
                [init_table, fn, expr, env],

                [lambda, [table],
                 [apply_pattern,
                  [make_pattern, [quote, [[lambda|[p|e]]|env]], [quote, [p, e, env]]],
                  table]
                ]
               ]
              ],

              [[eq, [car, fn], [quote, lambda]],
               [with_match,
                fn,
                [make_pattern, [quote, [lambda|[p|e]]], [quote, [p,e]]],
                [init_table, fn, expr, env],

                [lambda, [table],
                 [with_match,
                  [eval_list, expr, env],
                  [make_pattern, [quote, [args1|env1]], [quote, [args1, env1]]],
                  table,

                  [lambda, [table],
                   [with_match,
                    [apply,
                     [cdr, [subst, [quote, e], table]],
                     [append,
                      [zip,
                       [cdr, [subst, [quote, p], table]],
                       [cdr, [subst, [quote, args1], table]]
                      ],
                      [cdr, [subst, [quote, env1], table]]
                     ]
                    ],
                    [make_pattern, [quote, [v|ignore]], [quote, [v]]],
                    table,

                    [lambda, [table],
                     [apply_pattern,
                      [make_pattern, [quote, [v|env1]], [quote, [v,env1]]],
                      table]
                    ]
                   ]
                  ]

                 ]
                ]
               ]
              ]

             ]
            ]
           ],

           [label, new_env,
            [lambda, [],
             [quote,
              [
               [quote|[fn|quote]],
               [atom|[fn|atom]],
               [eq|[fn|eq]],
               [car|[fn|car]],
               [cdr|[fn|cdr]],
               [cons|[fn|cons]],
               ['cond'|[fn|'cond']],
               [lambda|[fn|lambda]],
               [label|[fn|label]]
              ]
             ]]
           ],

           [label, eval,
            [lambda, [list],
             [last,
              [list2,
               [label, result, [eval_list, list, [new_env]]],
               [last, [car, result]]
              ]
             ]
            ]
           ]
          ],
          new_env()),
    Env.


eval(List) ->
    {V, _} = eval(List, init_env()),
    V.


eval2(List) ->
    {data, Value} = eval([[eval, [quote, List]]]),
    Value.


test('not') ->
    {data, false} = eval([['not', [quote, true]]]),
    {data, true} = eval([['not', [quote, false]]]);
test('and') ->
    {data, true} = eval([['and', [quote, true], [quote, true]]]),
    {data, false} = eval([['and', [quote, true], [quote, false]]]);
test(last) ->
    {data, c} = eval([[last, [quote, [a,b,c]]]]);
test(member) ->
    {data, true} = eval([[member, [quote, a], [quote, [a,b,c]]]]),
    {data, false} = eval([[member, [quote, c], [quote, [a,b]]]]);
test(append) ->
    {data, [a,b,c,d,e,f]} = eval([[append, [quote, [a,b,c]], [quote, [d,e,f]]]]);
test(zip) ->
    {data, [[a|d],[b|e],[c|f]]} = eval([[zip, [quote, [a,b,c]], [quote, [d,e,f]]]]);
test(subst) ->
    {data, [ok|c]} = eval([[subst, [quote, a], [quote, [[b|d],[a|c]]]]]),
    {data, none} = eval([[subst, [quote, a], [quote,[[b|d]]]]]),
    {data, [ok|c]} = eval([[subst, [quote, a], [quote, [[a|c],[b|d],[a|e]]]]]);
test(equal) ->
    {data, true} = eval([[equal, [quote, [a,b,c]], [quote, [a,b,c]]]]),
    {data, false} = eval([[equal, [quote, [a,b,c]], [quote, [a,b,c,d]]]]),
    {data, false} = eval([[equal, [quote, [a,b,c]], [quote, []]]]);
test(make_pattern) ->
    {data, nil} =
        eval([[make_pattern,
               [quote, []],
               [quote, []]]]),
    {data, [atom|a]} =
        eval([[make_pattern,
               [quote, a],
               [quote, []]]]),
    {data, [var|a]} =
        eval([[make_pattern,
               [quote, a],
               [quote, [a]]]]),
    {data, [cons|[[atom|a]|nil]]} =
        eval([[make_pattern,
               [quote, [a]],
               [quote, []]]]);
test(match) ->
    {data, []} =
        eval([[match,
               [quote, a],
               [quote, [atom|a]],
               [quote, []]]]),
    {data, [[a|a]]} =
        eval([[match,
               [quote, a],
               [quote, [var|a]],
               [quote, []]]]),
    {data, [[a|a]]} =
        eval([[match,
               [quote, a],
               [quote, [var|a]],
               [quote, [[a|a]]]]]),
    {data, false} =
        eval([[match,
               [quote, b],
               [quote, [var|a]],
               [quote, [[a|a]]]]]),
    {data, []} =
        eval([[match,
               [quote, a],
               [quote, ignore],
               [quote, []]]]),
    {data, []} =
        eval([[match,
               [quote, [a|b]],
               [quote, [cons|[[atom|a]|[atom|b]]]],
               [quote, []]]]);
test(apply_pattern) ->
    {data, []} =
        eval([[apply_pattern,
               [quote, nil],
               [quote, []]]]),
    {data, [a]} =
        eval([[apply_pattern,
               [quote, [cons|[[atom|a]|nil]]],
               [quote, []]]]),
    {data, [b]} =
        eval([[apply_pattern,
               [quote, [cons|[[var|a]|nil]]],
               [quote, [[a|b]]]]]);
test(quote) ->
    [data|a] = eval2([[quote,a]]),
    [data|[a,b,c]] = eval2([[quote,[a,b,c]]]);
test(atom) ->
    [data|true] = eval2([[atom,[quote,a]]]),
    [data|false] = eval2([[atom,[quote,[a,b,c]]]]),
    [data|false] = eval2([[atom,[quote,[]]]]);
test(eq) ->
    [data|true] = eval2([[eq,[quote,a],[quote,a]]]),
    [data|false] = eval2([[eq,[quote,a],[quote,b]]]),
    [data|true] = eval2([[eq,[quote,[]],[quote,[]]]]);
test(list) ->
    [data|a] = eval2([[car,[quote,[a,b,c]]]]),
    [data|[b,c]] = eval2([[cdr,[quote,[a,b,c]]]]),
    [data|[a,b,c]] = eval2([[cons,[quote,a],[quote,[b,c]]]]);
test('cond') ->
    [data|second] =
        eval2(
          [['cond',
            [[eq,[quote,a],[quote,b]],
             [quote,first]],
            [[atom,[quote,a]],
             [quote,second]]]]);
test(label) ->
    [data|a] =
        eval2(
          [[label, a, [quote, a]],
           a]),
    [data|[a,b,c]] =
        eval2(
          [[label, a, [quote, [a,b,c]]],
           a]);
test(lambda) ->
    [data|[a,b]] =
        eval2(
          [[[lambda,[x],
             [cons,x,[quote,[b]]]],
            [quote,a]]]),
    [data|[z,b,c]] =
        eval2(
          [[[lambda,[x,y],
             [cons,x,[cdr,y]]],
            [quote,z],
            [quote,[a,b,c]]]]),
    [data|[a,b,c]] =
        eval2(
          [[[lambda,[f],
             [f,[quote,[b,c]]]],
            [lambda,[x],
             [cons,[quote,a],x]]]]).

test() ->
    test('not'),
    test('and'),
    test(last),
    test(member),
    test(append),
    test(zip),
    test(subst),
    test(equal),
    test(make_pattern),
    test(match),
    test(apply_pattern),
    test(quote),
    test(atom),
    test(eq),
    test(list),
    test('cond'),
    test(label),
    test(lambda),
    ok.
