%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. kwi 2019 22:23
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Mateusz").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getStationDailyMean/4, start/0, loop/1, init/0, getMsg/0, tests/0]).

-record(station, {name, coordinates, measurements}).

-record(measurement, {type, value, timestamp}).








createMonitor() -> [].








addStation(StationName, Coords, []) -> [Station=#station{name=StationName, coordinates=Coords, measurements=[]}];
addStation(StationName, Coords, [Head | Tail]) ->
  Station=#station{name=StationName, coordinates=Coords, measurements=[]},
  if
    ((Head#station.name == StationName) or
      (Head#station.coordinates == Coords)) -> {"error", "station with given name/coordinates already exists"};
    true -> [Head | addStation(StationName, Coords, Tail)]
  end.

addMeasurement(Measurement, [Head | Tail]) ->
  if
    ((Head#measurement.type == Measurement#measurement.type) and
      (Head#measurement.timestamp == Measurement#measurement.timestamp)) -> {"error", "measurement with same station, type and date already exists"};
    true -> [Head | addMeasurement(Measurement, Tail)]
  end;
addMeasurement(Measurement, []) -> [Measurement].


addValue(Identifier, Time, Type, Value, Monitor) ->
  Measurement=#measurement{type = Type, timestamp = Time, value = Value},
  addValueMeasurement(Identifier, Measurement, Monitor).

addValueMeasurement(Identifier, Measurement, [Head | Tail]) ->
  if ((Head#station.coordinates == Identifier) or
    (Head#station.name == Identifier)) -> [Head#station{measurements = addMeasurement(Measurement, Head#station.measurements)} | Tail];
    true -> [Head | addValueMeasurement(Identifier, Measurement, Tail)]
  end;
addValueMeasurement(_, Measurement, []) -> {"error", "no matching station"}.

removeMeasurement(Timestamp, Type, [Head | Tail]) ->
  if
    ((Head#measurement.type == Type) and
      (Head#measurement.timestamp == Timestamp)) -> Tail;
    true -> [Head | removeMeasurement(Timestamp, Type, Tail)]
  end;
removeMeasurement(_, _, []) -> [].

removeValue(Identifier, Date, Type, [Head | Tail]) ->
  if ((Head#station.coordinates == Identifier) or
    (Head#station.name == Identifier)) -> [Head#station{measurements = removeMeasurement(Date, Type, Head#station.measurements)} | Tail];
    true -> [Head | removeValue(Identifier, Date, Type, Tail)]
  end;
removeValue(_, _, _, []) -> [].

getMeasurementValue(Type, Time, [Head | Tail]) ->
  if
    ((Head#measurement.type == Type) and
      (Head#measurement.timestamp == Time)) -> Head#measurement.value;
    true -> getMeasurementValue(Type, Time, Tail)
  end;
getMeasurementValue(_, _, []) -> {"error", "no matching measurement found"}.

getOneValue(Type, Date, Identifier, [Head | Tail]) ->
  if ((Head#station.coordinates == Identifier) or
    (Head#station.name == Identifier)) -> getMeasurementValue(Type, Date, Head#station.measurements);
    true -> getOneValue(Type, Date, Identifier, Tail)
  end;
getOneValue(_, _, _, []) -> {"error", "no matching station found"}.

getAvgMeasurement(Type, [Head | Tail], Count, Total) ->
  if (Head#measurement.type == Type)
    -> getAvgMeasurement(Type, Tail, Count+1, Total+Head#measurement.value);
    true -> getAvgMeasurement(Type, Tail, Count, Total)
  end;
getAvgMeasurement(_, [], 0, Total) -> {"error", "no measurements of given type"};
getAvgMeasurement(_, [], Count, Total) -> Total/Count.

getStationMean(Type, Identifier, [Head | Tail]) ->
  if ((Head#station.coordinates == Identifier) or
    (Head#station.name == Identifier)) -> getAvgMeasurement(Type, Head#station.measurements, 0, 0);
    true -> getStationMean(Type, Identifier, Tail)
  end;
getStationMean(_, _, []) -> {"error", "no matching station found"}.

getDailyMean(Type, Day, Monitor) ->
  getDailyMeanRecursive(Type, Day, Monitor, 0, 0).

getDailyMeanRecursive(Type, Day, [Head | Tail], Count, Total) ->
  DailyStationMean = getDailyStationMean(Type, Day, Head#station.measurements, 0, 0),
  if (DailyStationMean == 0) -> getDailyMeanRecursive(Type, Day, Tail, Count, Total);
    true -> getDailyMeanRecursive(Type, Day, Tail, Count+1, Total+DailyStationMean)
  end;
getDailyMeanRecursive(_, _, [], 0, _) -> {"error", "no matching measurements"};
getDailyMeanRecursive(_, _, [], Count, Total) -> Total/Count.

getDailyStationMean(Type, Day, [Head | Tail], Count, Total) ->
  if ((element(1, Head#measurement.timestamp)==Day) and
    (Head#measurement.type == Type)) -> getDailyStationMean(Type, Day, Tail, Count+1, Total+Head#measurement.value);
    true -> getDailyStationMean(Type, Day, Tail, Count, Total)
  end;
getDailyStationMean(_, _, [], 0, _) -> 0;
getDailyStationMean(_, _, [], Count, Total) -> Total/Count.

getStationDailyMean(Type, Day, Identifier, [Head | Tail]) ->
  if ((Head#station.coordinates == Identifier) or
    (Head#station.name == Identifier)) -> getDailyStationMean(Type, Day, Head#station.measurements, 0, 0);
    true -> getStationDailyMean(Type, Day, Identifier, Tail)
  end;
getStationDailyMean(_, _, _, []) -> {"error", "no station found"}.

terminate () -> ok.


handle(Monitor, {Task, Args}) ->
  case Task of
    addStation -> addStation(lists:nth(1,Args), lists:nth(2,Args), Monitor);
    addValue -> addValue(lists:nth(1,Args), lists:nth(2,Args), lists:nth(3,Args), lists:nth(4,Args), Monitor);
    removeValue -> removeValue(lists:nth(1,Args), lists:nth(2,Args), lists:nth(3,Args), Monitor);
    getOneValue -> getOneValue(lists:nth(1,Args), lists:nth(2,Args), lists:nth(3,Args), Monitor);
    getStationMean -> getStationMean(lists:nth(1,Args), lists:nth(2,Args), Monitor);
    getDailyMean -> getDailyMean(lists:nth(1,Args), lists:nth(2,Args), Monitor);
    getMonitor -> Monitor
  end.

loop(State) ->
  receive
    {From, Msg} ->
      NewState = handle(State, Msg),
      From ! NewState,
      case NewState of
        {"error", _} -> loop(State);
        _ -> loop(NewState)
      end;
    stop ->
      terminate()
  end.

init() ->
  State = createMonitor(),
  loop(State).

start() ->
  register(pollutionserver, spawn(pollution_server, init, [])).


getMsg() ->
  receive
    Msg -> Msg
  after
    1000 -> noMsg
  end.

stop() ->
  pollutionserver ! stop,
  unregister(pollutionserver).

flush() ->
  receive
    _ -> flush()
  after
    10 -> ok
  end.

tests() ->
  start(),
  pollutionserver ! {self(), {addStation, ["Aleja Slowackiego", {50.2345, 18.3445}]}},
  pollutionserver ! {self(), {addValue, [{50.2345, 18.3445}, calendar:local_time(), "PM10", 59]}},
  pollutionserver ! {self(), {removeValue, [{50.2345, 18.3445}, calendar:local_time(), "PM10"]}},
  pollutionserver ! {self(), {addValue, [{50.2345, 18.3445}, calendar:local_time(), "PM10", 61]}},
  pollutionserver ! {self(), {addStation, ["Aleja Myckiewicza", {16.13123, 10.1111}]}},
  pollutionserver ! {self(), {addValue, ["Aleja Myckiewicza", calendar:local_time(), "PM2,5", 123]}},
  flush(),
  pollutionserver ! {self(), {addValue, ["Aleja Slowackiego", calendar:local_time(), "PM2,5", 113]}},
  stop(),
  getMsg().
