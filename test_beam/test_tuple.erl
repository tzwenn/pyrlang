-module(test_tuple).
-export([test/0]).

test() ->
	P = {adam, 24, {july,29}},
	element(3,P).
