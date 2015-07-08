-module(mean_nnc).
-export([test/0, mean/1]).

mean(N) ->
  sum(duplicate(N, math:pi()))/N.

duplicate(N, X) ->
  duplicate(N, X, []).

duplicate(0, _, Acc) -> Acc;
duplicate(N, X, Acc) -> duplicate(N-1, X, [X|Acc]).

sum(X) -> sum(X, 0).
sum([], Acc) -> Acc;
sum([X|Xs], Acc) -> sum(Xs, X+Acc).


loop(0,R) -> R;
loop(N,_) -> loop(N-1,mean(10000000)).

test() ->
    loop(1,0).
