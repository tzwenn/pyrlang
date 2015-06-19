-module(cpstak).
-export([test/3, test/0, run_benchmark/1]).

cpstak(X,Y,Z) ->
	Tak = fun Tak(X,Y,Z,K) ->
			case Y < X of
				true -> Tak(X-1,Y,Z,
							fun(V1) ->
									Tak(Y-1,Z,X,
										fun(V2) ->
												Tak(Z-1,X,Y,
													fun(V3) ->
															Tak(V1,V2,V3,K)
													end)
										end)
							end);
				false -> K(Z)
			end
			end,
	Tak(X,Y,Z, fun(A) -> A end).

test(X,Y,Z) -> cpstak(X,Y,Z).

test() -> test(18,12,6).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
