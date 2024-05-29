-module(randomElements).
-export([random_elems/3, compare_speeds/3]).


random_elems(N, Min, Max) -> [Min - 1 + rand:uniform(Max - Min + 1) || _<-lists:seq(1, N)].

compare_speeds(List, Fun1, Fun2) -> 
    {Time1, _} = timer:tc(Fun1, List),
    {Time2, _} = timer:tc(Fun2, List),
    io:format("~p~n", [Time1]),
    io:format("~p~n", [Time2]),
    if 
        Time1 > Time2 -> 1;
        Time1 < Time2 -> -1;
        true -> 0
    end.
