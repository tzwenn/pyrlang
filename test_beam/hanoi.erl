-module(hanoi).
-export([test/1]).

hanoi(1) -> 1;
hanoi(N) ->
	2 * hanoi(N-1)+1.

loop(0, Res) -> Res;
loop(N, Res) ->
	loop(N-1, hanoi(28)).

test(N) ->
	erlang:display(loop(N, 0)).
