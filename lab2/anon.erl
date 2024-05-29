-module('anon').
-export([]).

fun (Str) -> lists:map((X)-> case X of "o" -> "a"; "e" -> "o"; _ -> X end, Str) end.
