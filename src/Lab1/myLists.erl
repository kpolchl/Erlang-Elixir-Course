-module(myLists).
-export([contains/2,duplicate/1,sumFloats/2]).

contains([],_A) -> false;
contains([A|_],A) -> true;
contains([_|T],A) -> contains(T,A).

duplicate([]) -> [];
duplicate([H|T]) -> [H,H|duplicate(T)].

sumFloats([],Sum) -> Sum;
sumFloats([H|T],Sum) -> sumFloats(T,Sum+H).

