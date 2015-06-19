-module(fib).
-export([test/1, test/0, run_benchmark/1]).

fib(N) when N < 2 -> 1;
fib(N) -> fib(N-1) + fib(N-2).

test(N) -> fib(N).

test() -> test(35).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
