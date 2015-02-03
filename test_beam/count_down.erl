-module(count_down).
-export([test/1]).

cd(0) -> 0;
cd(N) -> test(N - 1).

test(N) ->
	cd(N),
	cd(N).

