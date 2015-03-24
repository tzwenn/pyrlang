-module(recursive).
-export([test/1, range_sum/1, range_sum_tl/1]).

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
