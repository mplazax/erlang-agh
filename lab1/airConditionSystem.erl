-module(airConditionSystem).


%% API
-export([get_readings/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).


get_readings() ->
  [
    {"Aleja Pokoju", {{2024, 3, 2}, {17, 51, 11}}, [
      {pm10, 20},
      {pm25, 15},
      {temperature, 25}
    ]},
    {"Aleja Pokoju",  {{2024, 4, 2}, {17, 51, 11}}, [
      {pm10, 18},
      {pm25, 12},
      {temperature, 24},
      {humidity, 60}
    ]},
    {"Aleja Pokoju",  {{2024, 5, 2}, {17, 51, 11}}, [
      {pm10, 22},
      {pm25, 17},
      {pm1, 10},
      {pressure, 1013}
    ]},
    {"Czarnowiejska",  {{2024, 3, 2}, {17, 51, 11}}, [
      {pm10, 21},
      {pm25, 16},
      {temperature, 26}
    ]},
    {"Czarnowiejska",  {{2024, 4, 2}, {17, 51, 11}}, [
      {pm10, 19},
      {pm25, 14},
      {temperature, 23},
      {humidity, 65}
    ]},
    {"Czarnowiejska",  {{2024, 5, 2}, {17, 51, 11}}, [
      {pm10, 23},
      {pm25, 18},
      {pm1, 11},
      {pressure, 1015}
    ]},

    {"Pawia",  {{2024, 2, 2}, {17, 51, 11}}, [
      {pm10, 19},
      {pm25, 14},
      {temperature, 23}
    ]},
    {"Pawia",  {{2024, 4, 2}, {17, 51, 11}}, [
      {pm10, 17},
      {pm25, 13},
      {temperature, 22},
      {humidity, 62}
    ]},
    {"Pawia",  {{2024, 4, 2}, {17, 51, 11}}, [
      {pm10, 20},
      {pm25, 15},
      {pm1, 9},
      {pressure, 1010}
    ]}
  ].

%% first argument is readings array and second is date
number_of_readings([{_,{{Y1, M1, D1}, _}, _}|T], {Y2, M2, D2}) when (Y1 == Y2) and (M1 == M2) and (D1 == D2) -> 1 + number_of_readings(T, {Y2, M2, D2});
number_of_readings([], _) -> 0;
number_of_readings([_|T], Date) -> number_of_readings(T, Date). 

%% finds value of measurment of a given type from a reading
find_value_of_a_type({_, _, [{T1, Val}|_]}, T2) when T1 == T2 -> Val;
find_value_of_a_type({_, _, []}, _) -> 0;
find_value_of_a_type({Name, DateTime, [_|T]}, T2) -> find_value_of_a_type({Name, DateTime, T}, T2).

maximum(A, B) when A > B -> A;
maximum(_, B) -> B.

calculate_max([], _) -> 0;
calculate_max([R|T], Type) -> maximum(find_value_of_a_type(R, Type), calculate_max(T, Type)).

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

len([_|T]) -> 1 + len(T);
len([]) -> 0.

mean([]) -> 0;
mean(Array) -> sum(Array) / len(Array).

find_values_of_a_type([H|T], Type) -> [find_value_of_a_type(H, Type)|find_values_of_a_type(T, Type)];
find_values_of_a_type([], _) -> [].

calculate_mean(Readings, Type) -> mean(find_values_of_a_type(Readings, Type)).
