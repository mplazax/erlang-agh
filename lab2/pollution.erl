-module(pollution).

-export([]).

-record(coords, {x, y}).
-record(date, {year, month, day}).
-record(time, {hour, minute, second}).
-record(station, {name, coords=#coords{}}).
-record(dtime, {date = #date{}, time = #time{}}).
-record(msmt, {dtime= #dtime{}, type, value}).

%% monitor is a map like key: station, value: list[msmt]
create_monitor() -> maps:new().


add_station(Monitor, Station) ->
    IsPresent = maps:is_key(Station, Monitor),
    case IsPresent of
        true -> {error, "add_station: station already exists"};
        false -> maps:put(Station, [], Monitor)
    end.


add_value(Dtime, Type, Value, Station, Monitor) ->
    Msmt = #msmt{dtime = Dtime, type = Type, value = Value},
    DoesStationExist = maps:is_key(Station, Monitor),
    case DoesStationExist of
        false -> {error, "add_value: station does not exist"};
        true -> 
            CurrMsmts = maps:get(Station, Monitor),
            case can_append_msmt(Msmt, CurrMsmts) of
                true -> maps:put(Station, [Msmt|CurrMsmts], Monitor);
                false -> {error, "add_value: cannot append measurement"}
            end
    end.


can_append_msmt(Msmt, [H|T]) -> 
    AreDtimesEqual = Msmt#msmt.dtime == H#msmt.dtime,
    AreTypesEqual = Msmt#msmt.type == H#msmt.type,
    case AreTypesEqual and AreDtimesEqual of
        true -> false;
        false -> can_append_msmt(Msmt, T)
    end;
can_append_msmt(_Msmt, []) -> true.


remove_value(Dtime, Type, Station, Monitor) -> 
    DoesStationExist = maps:is_key(Station, Monitor),
    case DoesStationExist of
        false -> {error, "remove_value: station does not exist"};
        true ->
            NewMsmts = [X || X <- maps:get(Station, Monitor)],
            maps:put(Station, NewMsmts, Monitor)
    end.


get_one_value(Station, Type, Dtime, Monitor) ->
    Value = get_msmt(Type, Dtime, maps:get(Station, Monitor)),
    case Value of
        {error, _} -> {error, "get_one_value: no measurement found for given station, type, dtime"};
        _ -> Value
    end.


get_msmt(Type, Dtime, Msmts) ->
    MsmtAsList = [X || X <- Msmts, X#msmt.dtime == Dtime, X#msmt.type == Type],
    case MsmtAsList of 
        [] -> {error, "get_msmt: no measurement with dtime and type found"};
        _ -> 
            [Msmt|_] = MsmtAsList,
            Msmt#msmt.value
    end.


sum(List) -> lists:foldl(fun(X, Acc) -> X + Acc, 0, List).


get_station_mean(Station, Type, Monitor) ->
    Msmts = maps:get(Station, Monitor),
    ValuesOfType = [X#msmt.value || X <- Msmts, X#msmt.type == Type],
    case ValuesOfType of
        [] -> {error, "get_station_mean: no measurements of such type for given station"};
        _ -> 
            Sum = sum(ValuesOfType),
            Sum / length(ValuesOfType)
    end. 


get_daily_mean(Date, Type, Monitor) ->
    AllMsmts = lists:foldl(fun(X, Acc) -> X ++ Acc, [], maps:values(Monitor)),
    ValuesOfTypeDate = [X#msmt.value || X <- AllMsmts, X#msmt.dtime#dtime.date == Date, X#msmt.type == Type],
    case ValuesOfTypeDate of 
        [] -> {error, "get_daily_mean: no measurements of such date and type for any stations"};
        _ -> 
            Sum = sum(ValuesOfTypeDate),
            Sum / length(ValuesOfTypeDate)
    end.


get_station_by_coords(Coords, Monitor) ->
    Stations = maps:keys(Monitor),
    [Station|_] = [S || S <- Stations, S#station.coords == Coords],
    Station.


is_in_last_24h(Msmt, Dtime) ->
    case Msmt#msmt.dtime#dtime.date == Dtime#dtime.date of
        false -> false;
        true ->
            %% msmt was before dtime
            case Msmt#msmt.dtime#dtime.time#time.hour < Dtime#dtime.time#time.hour of
                true -> true;
                false -> false
            end
    end.


get_hour_delta(Msmt, Dtime) -> 
    Dtime#dtime.time#time.hour - Msmt#msmt.dtime#dtime.time#time.hour.


get_moving_mean(Type, Dtime, Coords, Monitor) ->
    Station = get_station_by_coords(Coords, Monitor),
    Msmts = maps:get(Station, Monitor),
    MsmtsOfType = [X || X <- Msmts, X#msmt.type == Type],
    MsmtsOfTypeLast24H = [X || X <- MsmtsOfType, is_in_last_24h(X, Dtime)],
    ValsZipWts = [{X, 24 - get_hour_delta(X, Dtime)} || X <- MsmtOfTypeLast24H],
    TotalWt = lists:foldl(fun({_, Wt}, Acc) -> Acc + Wt, 0, ValsZipWts),
    TotalVal = lists:foldl(fun({Val, _}, Acc) -> Acc + Val, 0, ValsZipWts),
    case TotalWt of 
        0 -> {error, "get_moving_mean: no measurements "};
        _ -> TotalVal / TotalWt
    end.

