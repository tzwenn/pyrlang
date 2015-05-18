-module(timer).
-export([tc/1,tc/2,tc/3,now_diff/2]).
%%
%% Measure the execution time (in microseconds) for Fun().
%%
-spec tc(Fun) -> {Time, Value} when
      Fun :: function(),
      Time :: integer(),
      Value :: term().
tc(F) ->
    Before = os:timestamp(),
    Val = F(),
    After = os:timestamp(),
    {now_diff(After, Before), Val}.

%%
%% Measure the execution time (in microseconds) for Fun(Args).
%%
-spec tc(Fun, Arguments) -> {Time, Value} when
      Fun :: function(),
      Arguments :: [term()],
      Time :: integer(),
      Value :: term().
tc(F, A) ->
    Before = os:timestamp(),
    Val = apply(F, A),
    After = os:timestamp(),
    {now_diff(After, Before), Val}.

%%
%% Measure the execution time (in microseconds) for an MFA.
%%
-spec tc(Module, Function, Arguments) -> {Time, Value} when
      Module :: module(),
      Function :: atom(),
      Arguments :: [term()],
      Time :: integer(),
      Value :: term().
tc(M, F, A) ->
    Before = os:timestamp(),
    Val = apply(M, F, A),
    After = os:timestamp(),
    {now_diff(After, Before), Val}.

%%
%% Calculate the time difference (in microseconds) of two
%% erlang:now() timestamps, T2-T1.
%%
-spec now_diff(T2, T1) -> Tdiff when
      T1 :: erlang:timestamp(),
      T2 :: erlang:timestamp(),
      Tdiff :: integer().
now_diff({A2, B2, C2}, {A1, B1, C1}) ->
    ((A2-A1)*1000000 + B2-B1)*1000000 + C2-C1.


