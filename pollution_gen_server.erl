%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. cze 2019 00:22
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("Mateusz").

%% API

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/0, start_link/0, stop/0, addStation/2, addValue/4, getOneValue/3, crash/0]).

-record(station, {name, coordinates, measurements}).

-record(measurement, {type, value, timestamp}).



createMonitor() ->
  timer:sleep(0),
  getLast([]).


getLast(Last) ->
  receive
    Any -> getLast(Any)
  after
    10 -> Last
  end.

start() ->
  gen_server:start({local, pollution_server}, ?MODULE, [], []).
start_link() ->
  gen_server:start_link({local, pollution_server}, ?MODULE, [], []).

stop() ->
  gen_server:stop(pollution_server).

init(_Args) ->
  {ok, createMonitor()}.

crash() ->
  gen_server:call(pollution_server, {crash}).
addStation(StationName, Coords) ->
  gen_server:call(pollution_server, {addStation, StationName, Coords}).
addValue(Identifier, Time, Type, Value) ->
  gen_server:call(pollution_server, {addValue, Identifier, Time, Type, Value}).
getOneValue(Type, Date, Identifier) ->
  gen_server:call(pollution_server, {getOneValue, Type, Date, Identifier}).

handle_call({addStation, StationName, Coords}, _From, State) ->
  NewState = addStationInternal(StationName, Coords, State),
  case NewState of
    {error, _} -> {reply, NewState, State};
    _ ->
      pollution_cache ! NewState,
      {reply, NewState, NewState}
  end;
handle_call({addValue, Identifier, Time, Type, Value}, _From, State) ->
  NewState = addValueInternal(Identifier, Time, Type, Value, State),
  case NewState of
    {error, _} -> {reply, NewState, State};
    _ ->
      pollution_cache ! NewState,
      {reply, NewState, NewState}
  end;
handle_call({crash}, _From, State) ->
  NewState = unexisting:crashme(),
  {reply, NewState, State};
handle_call({getOneValue, Type, Date, Identifier}, _From, State) ->
  NewState = getOneValueInternal(Type, Date, Identifier, State),
  case NewState of
    {error, _} -> {reply, NewState, State};
    _ ->
      pollution_cache ! NewState,
      {reply, NewState, NewState}
  end.
handle_cast(_Request, State) ->
  {noreply, State}.
handle_info(_Info, State) ->
  {noreply, State}.
terminate(_Reason, _State) ->
  ok.







addStationInternal(StationName, Coords, []) -> [Station=#station{name=StationName, coordinates=Coords, measurements=[]}];
addStationInternal(StationName, Coords, [Head | Tail]) ->
  Station=#station{name=StationName, coordinates=Coords, measurements=[]},
  if
    ((Head#station.name == StationName) or
      (Head#station.coordinates == Coords)) -> {"error", "station with given name/coordinates already exists"};
    true -> [Head | addStationInternal(StationName, Coords, Tail)]
  end.

addMeasurement(Measurement, [Head | Tail]) ->
  if
    ((Head#measurement.type == Measurement#measurement.type) and
      (Head#measurement.timestamp == Measurement#measurement.timestamp)) -> {"error", "measurement with same station, type and date already exists"};
    true -> [Head | addMeasurement(Measurement, Tail)]
  end;
addMeasurement(Measurement, []) -> [Measurement].


addValueInternal(Identifier, Time, Type, Value, Monitor) ->
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

getOneValueInternal(Type, Date, Identifier, [Head | Tail]) ->
  if ((Head#station.coordinates == Identifier) or
    (Head#station.name == Identifier)) -> getMeasurementValue(Type, Date, Head#station.measurements);
    true -> getOneValueInternal(Type, Date, Identifier, Tail)
  end;
getOneValueInternal(_, _, _, []) -> {"error", "no matching station found"}.

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