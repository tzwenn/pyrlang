-module(reverse).
-export([reverse/1]).
-export([seq/1]).
-export([start/1]).
-export([sort/1]).

seq(0) -> [];
seq(N) -> [N|seq(N-1)].

reverse(L) ->
	reverse_aux(L, []).

reverse_aux([], A) -> A;
reverse_aux([H|Rest], A) ->
	reverse_aux(Rest, [H|A]).

start(N) ->
	L = seq(N),
	erlang:display(L),
	sort(seq(N)).

sort(L) ->
	sort(L, [], true).

sort([], L, true) ->
	reverse(L);
sort([], L, false) ->
	sort(reverse(L), [], true);
sort([ X, Y | T ], L, _) when X > Y ->
	sort([ X | T ], [ Y | L ], false);
sort([ X | T ], L, Halt) ->
	sort(T, [ X | L ], Halt).
