-module(test_append_bif).
-export([test/0]).
foo() ->
	"asdhgasdg".

test() ->
	foo() ++ "gsadgsadg".

