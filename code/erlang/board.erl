-module(board).

-export([move/2, move_n/2]).

move(left, {X,Y})
  when X > 1, X =< 8 ->
    {X-1, Y};
move(right, {X,Y})
  when X >= 1, X < 8 ->
    {X+1, Y};
move(up, {X,Y})
  when Y > 1, Y =< 8 ->
    {X, Y-1};
move(down, {X,Y})
  when Y >= 1, Y < 8 ->
    {X, Y+1}.


move_n(From, []) ->
    From;
move_n(From, [H|T]) ->
    move_n(move(H, From), T).

