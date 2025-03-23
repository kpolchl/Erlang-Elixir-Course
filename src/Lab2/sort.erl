%%%-------------------------------------------------------------------
%%% @author paranoidppl
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Mar 2025 13:26
%%%-------------------------------------------------------------------
-module(sort).
-author("paranoidppl").

%% API
-export([less_than/2,grt_eq_than/2, qs/1,random_elems/3]).

less_than(List , Arg) -> [X || X <- List , X <Arg].

grt_eq_than(List, Arg) -> [X || X <- List , X >=Arg].

qs([]) -> [];
qs([A]) -> [A];
qs([Pivot|Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ).


random_elems(N,Min,Max) -> [rand:uniform(Max-Min+1) +Min-1 || _<-lists:seq(1,N-1)].

%%compare_speeds(List, Fun1, Fun2) ->
%%  {T1,_} -> time:tc(Fun1,[List]),
%%  {T2,_} -> time:tc(Fun2,[List]),
%%  case T1 > T2
%%

