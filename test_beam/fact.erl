-module(fact).
-export([fact/1, real_fact/1, test/0]).

test() ->
	fact(20000000).

fact(N) -> fact(N, 1).

fact(0, Acc) -> Acc;
fact(N, Acc) when N > 0 -> fact(N-1,N*(Acc rem 100 + 1)).

real_fact(N) -> real_fact(N, 1).

real_fact(0, Acc) -> Acc;
real_fact(N, Acc) when N > 0 -> 
	Next = N*Acc,
	erlang:display(Next),
	real_fact(N-1, Next).
