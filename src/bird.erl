-module(bird).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("flight.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE).
-define(SLEEP_TIME, 2000).

-record(state, { position,
                 direction,
                 range     = 2,
                 movement  = 1,
                 neighbors = [],
                 event_pid
               }).

-type state()    :: #state{}.
-type neighbor() :: {position(), direction()}.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  Position  = flight_world:random_position(),
  Direction = random_direction(),
  {ok, Pid} = bird_event:start_link(),
  bird_event:add_handler(self()),
  erlang:send_after(random_delay(), self(), trigger),
  {ok, #state{ position  = Position,
               direction = Direction,
               event_pid = Pid }}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(trigger, State) ->
  NewState = handle_trigger_event(State),
  erlang:send_after(random_delay(), self(), trigger),
  {noreply, NewState};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

random_delay() ->
  2000.

%% @private
%% @doc Move and notify the surrounding.
-spec handle_trigger_event(state()) -> state().
handle_trigger_event(#state{ position = From } = State) ->
  To        = determine_move(State),
  Direction = determine_direction(From, To),
  bird_event_manager:move(self(), From, To),
  State#state{ position = To, direction = Direction }.

%% @private
%% @doc Decide which is the best move we can make.
-spec determine_move(state()) -> position().
determine_move(State) ->
  DesiredDest = desired_destination(State),
  ValidMoves  = valid_moves(State),
  find_best_move(DesiredDest, ValidMoves).

%% @private
%% @doc Determine which direction you move in when going from one
%% point to another.
-spec determine_direction(position(), position()) -> direction().
determine_direction({FromX, FromY}, {ToX, ToY}) ->
  ToDirFun = fun(Change) ->
                 if Change > 0 ->
                     1;
                    Change < 0 ->
                     -1;
                    true ->
                     0
                 end
             end,
  {ToDirFun(ToX - FromX), ToDirFun(ToY - FromY)}.

%% @private
%% @doc Determine where we want to move.
-spec desired_destination(state()) -> position().
desired_destination(#state{ neighbors = [] } = State) ->
  #state{ position = Pos, direction = Dir } = State,
  calculate_destination(Pos, Dir);
desired_destination(#state{ neighbors = Neighbors,
                            position = Pos,
                            direction = Dir
                          }) ->
  NeighborDirections = neighbor_directions(Neighbors),
  Direction          = average_direction([Dir|NeighborDirections]),
  calculate_destination(Pos, Direction).

-spec find_best_move(position(), [position()]) -> position().
find_best_move(DesiredDest, ValidMoves) ->
  CalculateDistancesFun = fun(Pos, Acc) ->
                              Score = calculate_distance(DesiredDest, Pos),
                              orddict:append(Score, Pos, Acc)
                          end,
  Scores = lists:foldl(CalculateDistancesFun, orddict:new(), ValidMoves),
  {_BestScore, Candidates} = hd(lists:keysort(1, orddict:to_list(Scores))),
  pick_random(Candidates).

%% @private
%% @doc Pick a random element in a list.
-spec pick_random([position()]) -> position().
pick_random(Candidates) ->
  Len = length(Candidates),
  lists:nth(random:uniform(Len), Candidates).

%% @private
%% @doc Calculate where you'll end up if you move in the specified
%% direction.
-spec calculate_destination(position(), direction()) -> position().
calculate_destination({PosX, PosY}, {DirX, DirY}) ->
  {PosX + DirX, PosY + DirY}.

%% @private
%% @doc Get a list of free positions.
-spec valid_moves(state()) -> [position()].
valid_moves(#state{ neighbors = Neighbors,
                    position = Position,
                    movement = Movement
                  }) ->
  PossibleMoves = flight_world:valid_moves(Position, Movement),
  NeighborPositions = neighbor_positions(Neighbors),
  FilterFun = fun(Pos) ->
                  not lists:member(Pos, NeighborPositions)
              end,
  lists:filter(FilterFun, PossibleMoves).

%% @private
%% @doc Extract positions from a list of Neighbors.
-spec neighbor_positions([neighbor()]) -> [position()].
neighbor_positions(Neighbors) ->
  [ Position || {Position, _Direction} <- Neighbors ].

%% @private
%% @doc Extract directions from a list of Neighbors.
-spec neighbor_directions([neighbor()]) -> [direction()].
neighbor_directions(Neighbors) ->
  [ Direction || {_Position, Direction} <- Neighbors ].

%% @private
%% @doc Calculate the average from a list of directions.
-spec average_direction([direction()]) -> direction().
average_direction(Directions) ->
  {SumX, SumY} = lists:foldl(fun({X,Y}, {AccX, AccY}) ->
                                 {AccX + X, AccY + Y}
                             end, {0, 0}, Directions),
  NumDirections = length(Directions),
  {round(SumX / NumDirections), round(SumY / NumDirections)}.

%% @private
%% @doc Calculate the distance between two positions.
-spec calculate_distance(position(), position()) -> integer().
calculate_distance({FromX, FromY}, {ToX, ToY}) ->
  DistanceX = abs(FromX - ToX),
  DistanceY = abs(FromY - ToY),
  case {DistanceX, DistanceY} of
    {Dist, Dist} -> Dist;
    _            -> DistanceX + DistanceY
  end.

%% @private
%% @doc Get a random direction.
-spec random_direction() -> direction().
random_direction() ->
  Directions = [ {-1, -1}, {0, -1}, {1, -1},
                 {-1, 0},           {1, 0},
                 {-1, 1},  {0, 1},  {1, 1} ],
  lists:nth(random:uniform(length(Directions)), Directions).

%%%===================================================================
%%% EUnit tests
%%%===================================================================

-ifdef(TEST).

valid_move_test_setup() ->
  mock_flight_world(),
  [flight_world].

valid_move_test_cleanup(MeckedModules) ->
  unmock(MeckedModules).

valid_moves_test_() ->
  {setup, fun valid_move_test_setup/0, fun valid_move_test_cleanup/1,
   [?_test(
       begin
         Position = {2, 3},
         Neighbors = [{{2, 2}, n}, {{3, 4}, nw}],
         State = #state{ position = Position,
                         neighbors = Neighbors
                       },
         Expected = [{1, 3}, {2, 4}],
         ?assertEqual(Expected, valid_moves(State))
       end)]}.

average_direction_test_() ->
  [ ?_assertEqual({1, 1}, average_direction([{1, 1}])),
    ?_assertEqual({1, 0}, average_direction([{1, 1},
                                             {1, 1},
                                             {1, 0},
                                             {1, -1}]))
  ].

calculate_distance_test_() ->
  [ ?_assertEqual(2, calculate_distance({1, 1}, {3, 3})),
    ?_assertEqual(2, calculate_distance({3, 3}, {1, 1})),
    ?_assertEqual(3, calculate_distance({4, 4}, {7, 1})),
    ?_assertEqual(1, calculate_distance({3, 2}, {3, 3})),
    ?_assertEqual(1, calculate_distance({2, 3}, {3, 3})),
    ?_assertEqual(3, calculate_distance({1, 2}, {3, 3}))
  ].

determine_direction_test_() ->
  [ ?_assertEqual({0, 0},  determine_direction({1, 1}, {1, 1})),
    ?_assertEqual({0, 1},  determine_direction({1, 1}, {1, 3})),
    ?_assertEqual({1, 1},  determine_direction({1, 1}, {2, 2})),
    ?_assertEqual({-1, 1}, determine_direction({2, 2}, {1, 3}))
  ].

desired_destination_test_() ->
  NeighborA = {{2, 1}, {1, 1}},
  NeighborB = {{4, 2}, {1, 1}},
  NeighborC = {{3, 3}, {1, 0}},
  NeighborD = {{1, 4}, {1, -1}},
  NeighborE = {{3, 4}, {1, -1}},

  Position  = {2, 2},
  Direction = {1, -1},

  State = #state{ position  = Position,
                  direction = Direction
                },


  NoNeighbors = State#state{ neighbors = [] },
  OneNeighbor = State#state{ neighbors = [ NeighborA ] },
  AllNeighbors = State#state{ neighbors = [ NeighborA,
                                            NeighborB,
                                            NeighborC,
                                            NeighborD,
                                            NeighborE
                                          ] },
  [ ?_assertEqual({3, 1}, desired_destination(NoNeighbors)),
    ?_assertEqual({3, 2}, desired_destination(OneNeighbor)),
    ?_assertEqual({3, 2}, desired_destination(AllNeighbors))
  ].

find_best_move_setup() ->
  mock_random(),
  [random].

find_best_move_cleanup(MeckedModules) ->
  unmock(MeckedModules).

find_best_move_test_() ->
  {setup, fun find_best_move_setup/0, fun find_best_move_cleanup/1,
   [?_test(
       begin
         DesiredDest = {3, 3},
         ValidMoves  = [{1, 1},
                        {1, 2},
                        {3, 2},
                        {2, 3}
                       ],
         ?assertEqual({3, 2}, find_best_move(DesiredDest, ValidMoves))
       end)
   ]}.

determine_move_setup() ->
  mock_flight_world(),
  mock_random(),
  [flight_world, random].

determine_move_cleanup(MeckedModules) ->
  unmock(MeckedModules).

determine_move_test_() ->
  {setup, fun determine_move_setup/0, fun determine_move_cleanup/1,
   [?_test(
       begin
         NeighborA = {{2, 1}, {1, 1}},
         NeighborB = {{4, 2}, {1, 1}},
         NeighborC = {{3, 3}, {1, 0}},
         NeighborD = {{1, 4}, {1, -1}},
         NeighborE = {{3, 2}, {1, -1}},

         Position  = {2, 2},
         Direction = {1, -1},

         Neighbors = [ NeighborA,
                       NeighborB,
                       NeighborC,
                       NeighborD,
                       NeighborE
                     ],

         State = #state{ position  = Position,
                         direction = Direction,
                         neighbors = Neighbors
                       },

         %% we 'randomly' pick the first valid move
         ?assertEqual({2, 3}, determine_move(State))
       end)
   ]}.

random_direction_setup() ->
  mock_random(),
  [random].

random_direction_cleanup(MeckedModules) ->
  unmock(MeckedModules).

random_direction_test_() ->
  {setup, fun random_direction_setup/0, fun random_direction_cleanup/1,
   [?_test(
       begin
         ?assertEqual({-1, -1}, random_direction())
       end)
   ]}.

mock_flight_world() ->
  meck:new(flight_world),
  meck:expect(flight_world, valid_moves, fun({X, Y}, _Range) ->
                                             [{X, Y - 1},
                                              {X - 1, Y},
                                              {X, Y + 1},
                                              {X + 1, Y + 1}
                                             ]
                                         end).

mock_random() ->
  meck:new(random, [unstick, passthrough]),
  meck:expect(random, uniform, fun(_) -> 1 end).

unmock(MeckedModules) ->
  [meck:unload(Module) || Module <- MeckedModules].

-endif.
