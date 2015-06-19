-module(fpsum).
-export([test/0, run_benchmark/1]).

run() -> 
	run(1000000.0, 0.0).

run(I,N) when I < 0.0 -> N;
run(I,N) -> run(I-1.0,N+I).

test() -> run().

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
