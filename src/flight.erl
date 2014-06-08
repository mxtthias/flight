-module(flight).

-export([start/0, stop/0]).

start() ->
  flight_app:start().

stop() ->
  flight_app:stop([]).
