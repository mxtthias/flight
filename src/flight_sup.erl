-module(flight_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  World   = {flight_world, {flight_world, start_link, []},
	     permanent, 2000, worker, [flight_world]},
  BirdSup = {bird_sup, {bird_sup, start_link, []},
	     permanent, 2000, supervisor, [bird_sup]},
  Children = [World, BirdSup],
  RestartStrategy = {one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.

