-module(fact).
-export([fact/1, test/0]).

test() ->
	fact(20000000).

fact(N) -> fact(N, 1).

fact(0, Acc) -> Acc;
fact(N, Acc) when N > 0 -> fact(N-1,N*(Acc rem 100 + 1)).
