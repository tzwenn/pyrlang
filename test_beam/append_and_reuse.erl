-module(append_and_reuse).
-export([test/1]).

foo() -> "asdgsadg".

loop() ->
	loop("", 100).

loop(Res, 0) -> Res;
loop(Res, N) ->
	loop(Res ++ foo(), N-1).

test(0) -> true;
test(N) -> loop(), test(N-1).
