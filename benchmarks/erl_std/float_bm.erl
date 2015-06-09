-module(float_bm).
-export([test/1, test/0]).

float_add(Iter) ->
    float_add(Iter, 1.1, 3.1416).

float_add(0, A, B) -> ok;
float_add(Iter, A, B) ->
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    A+B,
    float_add(Iter-1, A, B).
    
float_sub(Iter) ->
    float_sub(Iter, 1.1, 3.1416).

float_sub(0, A, B) -> ok;
float_sub(Iter, A, B) ->
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    A-B,
    float_sub(Iter-1, A, B).

float_mul(Iter) ->
    float_mul(Iter, 1.1, 3.1416).

float_mul(0, A, B) -> ok;
float_mul(Iter, A, B) ->
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    A*B,
    float_mul(Iter-1, A, B).

float_div(Iter) ->
    float_div(Iter, 1.1, 3.1416).

float_div(0, A, B) -> ok;
float_div(Iter, A, B) ->
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    A/B,
    float_div(Iter-1, A, B).

test(N) -> 
	float_add(N),
	float_sub(N),
	float_mul(N),
	float_div(N).

test() ->
	test(200000).
