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
                 neighbors = orddict:new(),
                 event_pid
               }).

-type state()    :: #state{}.
-type neighbor() :: orddict:orddict().

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
  bird_event_manager:introduce(erlang:pid_to_list(self()), Position),
  erlang:send_after(random_delay(), self(), trigger),
  {ok, #state{ position  = Position,
               direction = Direction,
               event_pid = Pid }}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({introduce, Pid, Position, Direction}, State) ->
  NewState = maybe_add_neighbor(Pid, Position, Direction, State),
  {noreply, NewState};
handle_cast({move, Id, From, To}, State) ->
  NewState = handle_move(Id, From, To, State),
  {noreply, NewState};
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
  bird_event_manager:move(erlang:pid_to_list(self()), From, To),
  State#state{ position = To, direction = Direction }.

%% @private
%% @doc Handle a move notification.
handle_move(Id, From, To, State) ->
  case is_within_range(To, State) of
    true  -> update_neighbors(Id, From, To, State);
    false -> maybe_remove_neighbor(Id, State)
  end.

%% @private
%% @doc Remove entry from neighbors if it exists.
-spec maybe_remove_neighbor(pid(), state()) -> state().
maybe_remove_neighbor(Id, #state{ neighbors = Neighbors } = State) ->
  NewNeighbors = orddict:erase(Id, Neighbors),
  State#state{ neighbors = NewNeighbors }.

%% @private
%% @doc Add neighbor if within range.
-spec maybe_add_neighbor(pid(), position(), direction(), state()) -> state().
maybe_add_neighbor(Pid, Position, Direction, State) ->
  case is_within_range(Position, State) of
    true  -> add_neighbor(Pid, Position, Direction, State);
    false -> State
  end.

%% @private
%% @doc Add neighbor.
-spec add_neighbor(pid(), position(), direction(), state()) -> state().
add_neighbor(Pid, Position, Direction,
             #state{ neighbors = Neighbors } = State) ->
  NewNeighbors = orddict:store(Pid, {Position, Direction}, Neighbors),
  State#state{ neighbors = NewNeighbors }.

%% @private
%% @doc Add new or update existing neighbor.
-spec update_neighbors(pid(), position(), position(), state()) -> state().
update_neighbors(Id, From, To, State) ->
  Direction = determine_direction(From, To),
  add_neighbor(Id, To, Direction, State).

%% @private
%% @doc Determine if a position is within our range
-spec is_within_range(position(), state()) -> boolean().
is_within_range(OtherPos, #state{ position = MyPos, range = Range }) ->
  calculate_distance(MyPos, OtherPos) =< Range.

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
  case flight_world:is_valid_position(DesiredDest) of
    true  -> calculate_best_move(DesiredDest, ValidMoves);
    false -> pick_random(ValidMoves)
  end.

-spec calculate_best_move(position(), [position()]) -> position().
calculate_best_move(DesiredDest, ValidMoves) ->
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
  NeighborsList = orddict:to_list(Neighbors),
  [ Position || {_Id, {Position, _Direction}} <- NeighborsList ].

%% @private
%% @doc Extract directions from a list of Neighbors.
-spec neighbor_directions([neighbor()]) -> [direction()].
neighbor_directions(Neighbors) ->
  NeighborsList = orddict:to_list(Neighbors),
  [ Direction || {_Id, {_Position, Direction}} <- NeighborsList ].

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
         Neighbors0 = orddict:new(),
         Neighbors1 = orddict:store(a, {{2,2}, {0,-1}}, Neighbors0),
         Neighbors  = orddict:store(b, {{3,4}, {1,1}}, Neighbors1),
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

is_within_distance_test_() ->
  State = #state{ position = {1, 1}, range = 2 },
  [ ?_assertEqual(true,  is_within_range({2, 2}, State)),
    ?_assertEqual(true,  is_within_range({3, 3}, State)),
    ?_assertEqual(true,  is_within_range({3, 1}, State)),
    ?_assertEqual(false, is_within_range({3, 4}, State)),
    ?_assertEqual(false, is_within_range({1, 4}, State))
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

  Neighbors0 = orddict:new(),
  Neighbors1 = orddict:store(a, NeighborA, Neighbors0),
  Neighbors2 = orddict:store(b, NeighborB, Neighbors1),
  Neighbors3 = orddict:store(c, NeighborC, Neighbors2),
  Neighbors4 = orddict:store(d, NeighborD, Neighbors3),
  Neighbors  = orddict:store(e, NeighborE, Neighbors4),

  Position  = {2, 2},
  Direction = {1, -1},

  State = #state{ position  = Position,
                  direction = Direction
                },


  NoNeighbors  = State#state{ neighbors = Neighbors0 },
  OneNeighbor  = State#state{ neighbors = Neighbors1 },
  AllNeighbors = State#state{ neighbors = Neighbors },
  [ ?_assertEqual({3, 1}, desired_destination(NoNeighbors)),
    ?_assertEqual({3, 2}, desired_destination(OneNeighbor)),
    ?_assertEqual({3, 2}, desired_destination(AllNeighbors))
  ].

find_best_move_setup() ->
  mock_flight_world(),
  mock_random(),
  [flight_world, random].

find_best_move_cleanup(MeckedModules) ->
  unmock(MeckedModules).

find_best_move_test_() ->
  {setup, fun find_best_move_setup/0, fun find_best_move_cleanup/1,
   [?_test(
       begin
         DesiredDest = {2, 3},
         InvalidDest = {3, 3},
         ValidMoves  = [{1, 1},
                        {1, 2},
                        {3, 2},
                        {2, 3}
                       ],
         ?assertEqual({2, 3}, find_best_move(DesiredDest, ValidMoves)),
         ?assertEqual({1, 1}, find_best_move(InvalidDest, ValidMoves))
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

         Neighbors0 = orddict:new(),
         Neighbors1 = orddict:store(a, NeighborA, Neighbors0),
         Neighbors2 = orddict:store(b, NeighborB, Neighbors1),
         Neighbors3 = orddict:store(c, NeighborC, Neighbors2),
         Neighbors4 = orddict:store(d, NeighborD, Neighbors3),
         Neighbors  = orddict:store(e, NeighborE, Neighbors4),

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

handle_move_test_() ->
  Position   = {1, 1},
  Range      = 1,
  Neighbors0 = orddict:new(),
  Neighbors  = orddict:store(a, {{1, 2}, {1, 0}}, Neighbors0),
  State      = #state{ position  = Position,
                       range     = Range,
                       neighbors = Neighbors },
  ExistingNeighborWithinRange  = handle_move(a, {1, 2}, {2, 2}, State),
  ExistingNeighborOutsideRange = handle_move(a, {1, 3}, {1, 4}, State),
  NewNeighborWithinRange  = handle_move(b, {3, 3}, {2, 2}, State),
  NewNeighborOutsideRange = handle_move(b, {3, 3}, {3, 4}, State),
  [ ?_assertEqual(true,
        orddict:is_key(a, ExistingNeighborWithinRange#state.neighbors)),
    ?_assertEqual({{2, 2}, {1, 0}},
        orddict:fetch(a, ExistingNeighborWithinRange#state.neighbors)),
    ?_assertEqual(false,
        orddict:is_key(a, ExistingNeighborOutsideRange#state.neighbors)),
    ?_assertEqual(true,
        orddict:is_key(b, NewNeighborWithinRange#state.neighbors)),
    ?_assertEqual({{2, 2}, {-1, -1}},
        orddict:fetch(b, NewNeighborWithinRange#state.neighbors)),
    ?_assertEqual(false,
        orddict:is_key(b, NewNeighborOutsideRange#state.neighbors))
  ].

mock_flight_world() ->
  meck:new(flight_world),
  meck:expect(flight_world, valid_moves, fun({X, Y}, _Range) ->
                                             [{X, Y - 1},
                                              {X - 1, Y},
                                              {X, Y + 1},
                                              {X + 1, Y + 1}
                                             ]
                                         end),
  meck:expect(flight_world, is_valid_position, fun({2, 3}) -> true;
                                                  ({3, 2}) -> true;
                                                  (_)      -> false
                                               end).

mock_random() ->
  meck:new(random, [unstick, passthrough]),
  meck:expect(random, uniform, fun(_) -> 1 end).

unmock(MeckedModules) ->
  [meck:unload(Module) || Module <- MeckedModules].

-endif.
