-module(append).
-export([append/2, test/1]).

append(E, []) -> [E];
append(E, [H|T]) -> [H|append(E,T)].

loop(0, Res) -> Res;
loop(N, Res) ->
	L = lists:seq(1,10000),
	loop(N-1, append(81, L)).

test(N) ->
	loop(N, []).
