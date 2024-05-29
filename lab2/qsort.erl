-module(qsort).

-export([qs/1]).


less_than(List, Arg) -> [X || X <- List, X < Arg].

grt_eq_than(List, Arg) -> [X || X <- List, X >= Arg].

qs([Pivot|Tail]) -> qs(less_than(Tail, Pivot)) ++ [Pivot] ++ qs(grt_eq_than(Tail,Pivot)).

