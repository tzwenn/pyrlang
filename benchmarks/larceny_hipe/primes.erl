-module(primes).
-export([test/0, test/1, run_benchmark/1]).

interval_list(M, N) when M > N -> [];
interval_list(M, N) ->
	[M|interval_list(M+1,N)].

remove_multiples(_, []) -> [];
remove_multiples(N, [H|T]) ->
	case H rem N == 0 of
		 true -> remove_multiples(N, T);
		 false -> [H|remove_multiples(N, T)]
	end.

sieve([]) -> [];
sieve([H|T]) ->
	[H|sieve(remove_multiples(H, T))].

primes(N) ->
	sieve(interval_list(2,N)).

test() ->
	test(100).

test(N) ->
	primes(N).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
