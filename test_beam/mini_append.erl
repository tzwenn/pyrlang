-module(mini_append).
-export([test/1]).

foo() -> "asdgasdhgdsggfcvzxcvvbbsaqwrehsfdgdrstyrtrasfdvsgfartefsdagstgsasrt4y5htdbsar65ehdrfsdsafsdgdhfsrtawfdyw45tarf".

bar() -> foo() ++ "sadgdsfhgffvzcxbnraerrt43gszfddagdhfndsasfdagsfharetwfsgdsfdhatweta4t5gsdfaddgfsffds".

test(0) -> true;
test(N) ->
	bar(),
	test(N-1).
