-module(recursive).
-export([test/1, range_sum/1, range_sum_tl/1, unroll/1, unroll2/1, run_test/2]).

test(0) -> 1;
test(1) -> 1;
test(N) ->
	test(N - 1) + test(N - 2).

range_sum(0) -> 0;
range_sum(N) -> 
	N + range_sum(N - 1).

range_sum_tl(N) -> range_sum_tl(N, 0).

range_sum_tl(0, Res) -> Res;
range_sum_tl(N, Res) -> range_sum_tl(N - 1, Res + N).

unroll(0) -> 1;
unroll(1) -> 1;
unroll(2) ->
	1 + unroll(2 - 1);
unroll(3) -> 
	unroll(2 - 1) + unroll(2 - 2) + 1;
unroll(N) ->
	unroll(N - 2) + unroll(N - 3) + unroll(N - 3) + unroll(N - 4).

unroll2(N) ->
	if
		N < 2 -> 1;
		N == 2 -> 1 + unroll2(1);
		N == 3 -> unroll2(1) + unroll2(0) + 1;
		true -> unroll2(N - 2) + unroll2(N - 3) + unroll2(N - 3) + unroll2(N - 4)
	end.

now_diff({A2, B2, C2}, {A1, B1, C1}) ->
	((A2-A1)*1000000 + B2-B1)*1000000 + C2 - C1.

run_test(0, _) -> true;
run_test(Times, N) ->
	Before = now(),
	test(N),
	After = now(),
	erlang:display(Times),
	erlang:display(now_diff(After, Before)),
	run_test(Times-1, N).
