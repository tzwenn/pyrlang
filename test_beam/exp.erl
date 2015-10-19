-module(exp).
-export([ill/1]).

ill(N) -> 
	erlang:display(N+1),
	5 div N.
