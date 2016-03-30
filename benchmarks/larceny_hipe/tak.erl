-module(tak).
-export([test/0, test/3, run_benchmark/1]).

tak(X,Y,Z) when Y < X ->
	tak(tak(X-1, Y, Z), tak(Y-1, Z, X), tak(Z-1, X, Y));
tak(_,_,Z) -> Z.

test() ->
	test(18, 12, 6).

test(X, Y, Z) ->
	tak(X,Y,Z).


run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
