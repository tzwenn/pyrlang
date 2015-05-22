-module(string).
-import(lists, [sublist/3]).
-export([test/1]).

-define(Scale, 500000).

grow(S) ->
	S1 = "123" ++ S ++ "456" ++ S ++ "789",
	S2 = lists:sublist(S1, 1+length(S1) div 2, length(S1)+1) ++ lists:sublist(S1, 1, 1+(length(S1) div 2)),
	S2.

trial(N) ->
	S = "abcdef",
	trial(N, S).

trial(N, Res) when length(Res) > N -> length(Res);
trial(N, Res) ->
	Next = grow(Res),
	trial(N, Next).

test(N) ->
	test(N, 10, "").

test(N, 0, Res) -> Res;
test(N, I, Res) -> 
	Next = trial(N),
	test(N, I-1, Next).
