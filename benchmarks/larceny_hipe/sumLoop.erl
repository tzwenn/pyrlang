-module(sumLoop).
-export([test/0, test/1, run_benchmark/1]).

tail_rec(N) ->
	tail_rec(N, 0).

tail_rec(0, Sum) -> Sum;
tail_rec(N, Sum) ->
	tail_rec(N-1, Sum+1).

test(N) ->
	tail_rec(N).

test() ->
	test(100000000).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
