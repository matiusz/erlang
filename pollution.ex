


defmodule Pollution do
  @moduledoc false
  def import(filename \\ "pollution.csv") do
    lines = File.read!(filename) |> String.split("\n")
    if (length(lines)<=5900) do
      IO.puts("Not enough lines")
    end
    %{:datetime => timestamp, :location => location, :pollutionLevel => value} = parseLine(List.first(lines))
    stationName(location)
  end

  def parseLine(line) do
    [dateString, hourString,xCoordString,yCoordString,valueString] = line |> String.split(",")
    date = dateString |> String.split("-") |> Enum.reverse() |> Enum.map(fn (x)-> elem(Integer.parse(x), 0) end) |> :erlang.list_to_tuple
    hour = hourString |> String.split(":") |> Enum.map(fn (x)-> elem(Integer.parse(x), 0) end) |> :erlang.list_to_tuple
    xCoord = xCoordString |> Float.parse |> elem(0)
    yCoord = yCoordString |> Float.parse |> elem(0)
    value = valueString |> Integer.parse |> elem(0)
    parsedLine = %{:datetime => {date,hour}, :location => {xCoord, yCoord}, :pollutionLevel => value}
  end
  def stationName({xCoord, yCoord}) do
    string = "station_#{xCoord}_#{yCoord}"
  end
end
