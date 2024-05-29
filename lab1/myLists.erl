-module(myLists).

-export([contains/2, duplicateElements/1, sumFloats/1]).

contains([H|_], Val) when H == Val -> true;
contains([], _) -> false;
contains([_|T], Val) -> contains(T, Val).

duplicateElements([]) -> [];
duplicateElements([H|T]) -> [H,H] ++ duplicateElements(T).

sumFloats([]) -> 0;
sumFloats([H|T]) when is_float(H) -> H + sumFloats(T);
sumFloats([_|T]) -> sumFloats(T).

