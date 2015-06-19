-module(fibfp).
-export([test/1, test/0, test10/0, run_benchmark/1]).

fibfp(N) when N < 2.0 -> N;
fibfp(N) ->
	fibfp(N-1.0) + fibfp(N-2.0).

test(N) -> fibfp(N).

test() -> test(35.0).
test10() -> test(10.0).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
