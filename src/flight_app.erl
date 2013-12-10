-module(flight_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
  application:start(flight).

start(_StartType, _StartArgs) ->
  {A1, A2, A3} = erlang:now(),
  random:seed(A1, A2, A3),
  flight_sup:start_link().

stop(_State) ->
  ok.
