-module(deriv).
-export([test/0, test/1, run_benchmark/1]).

my_plus_dderiv(A) ->
	['+'|lists:map(fun(X) -> dderiv(X) end, tl(A))].

my_minus_dderiv(A) ->
	['-'|lists:map(fun(X) -> dderiv(X) end, tl(A))].

mul_dderiv(A) ->
	['*',A,['+'|lists:map(fun(A) -> 
								  ['/', dderiv(A), A] end, 
						  tl(A))]].

div_dderiv(A) ->
	['-', 
	 ['/', dderiv(hd(tl(A))), tl(tl(A))],
	 ['/', hd(tl(A)), 
	  ['*', hd(tl(tl(A))), hd(tl(tl(A))),
	   dderiv(hd(tl(tl(A))))]]].

dderiv(A) when is_list(A) ->
	case hd(A) of
		'+' -> my_plus_dderiv(A);
		'-' -> my_minus_dderiv(A);
		'*' -> mul_dderiv(A);
		'/' -> div_dderiv(A)
	end;
dderiv(A) ->
	case A == x of
		 true -> 1;
		 false -> 0
	end.

test(A) -> dderiv(A).

test() ->
	test(['+', ['*', '3', x, x], ['*', a, x, x], ['*', b, x], '5']).

run_benchmark([Arg]) -> run_benchmark(list_to_integer(Arg));
run_benchmark(0) -> true;
run_benchmark(N) -> test(),run_benchmark(N-1).
