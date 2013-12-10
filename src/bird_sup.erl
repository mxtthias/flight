-module(bird_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([add_bird/0, add_birds/0, add_birds/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_birds() ->
  add_birds(20).

add_birds(N) ->
  [ add_bird() || _ <- lists:seq(1, N) ].

add_bird() ->
  supervisor:start_child(?SERVER, []).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  Bird = {bird, {bird, start_link, []},
          temporary, brutal_kill, worker, [bird]},
  Children = [Bird],
  RestartStrategy = {simple_one_for_one, 0, 1},
  {ok, {RestartStrategy, Children}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

