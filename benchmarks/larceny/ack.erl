-module(ack).
-export([test/0, test/2, run_benchmark/1]).

ack(0, N) -> N+1;
ack(M, 0) -> ack(M-1,1);
ack(M, N) -> ack(M-1, ack(M, N-1)).

test() ->
	test(3,9).

test(M,N) -> ack(M,N).


run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
