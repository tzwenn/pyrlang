-module(odd).
-export([odd/1, even/1]).

odd(0) -> false;
odd(N) -> even(N-1).

even(0) -> true;
even(N) -> odd(N-1).
