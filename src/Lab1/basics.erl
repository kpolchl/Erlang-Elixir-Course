-module(basics).
-export([hello/0,pow/2]).


pow(_,0) -> 1;
pow(A,1) -> A;
pow(A,N) -> A * pow(A , N-1).

hello() ->
  io:format("Hello, World!~n", []).





