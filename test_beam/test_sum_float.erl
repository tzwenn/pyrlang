-module(test_sum_float).
-export([test/1, test_int/1, test_length/1, sum_int/1, check/1]).

get_float() -> math:pi().

test(Res, 0) -> Res;
test(Res, N) -> test(Res+3.1415926535, N-1).
test(N) ->
	test(0.0, N).

test_int(Res, 0) -> Res;
test_int(Res, N) -> test_int(Res+999, N-1).

test_int(N) ->
	test_int(0, N).


d(N) ->
	lists:duplicate(N, get_float()).

test_length(N) ->
	length(d(N)).

sum_int(N) ->
	lists:sum(lists:duplicate(N, 899999999)).

check([]) -> true;
check([H|T]) ->
	case H == get_float() of
		true -> check(T);
		false -> false
	end;
check(N) ->
	check(d(N)).
