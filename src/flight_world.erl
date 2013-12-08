-module(flight_world).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([valid_moves/2, is_valid_position/1, random_position/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("flight.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(SERVER, ?MODULE).

-record(state, { dimension_x = 20,
                 dimension_y = 20,
                 obstacles   = []
               }).

-type state()    :: #state{}.


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get the possible moves given the logic of the world.
-spec valid_moves(position(), pos_integer()) -> [position()].
valid_moves(Position, Range) ->
  gen_server:call(?SERVER, {valid_moves, Position, Range}).

-spec is_valid_position(position()) -> boolean().
is_valid_position(Position) ->
  gen_server:call(?SERVER, {is_valid_position, Position}).

%% @doc Get a random position in the world, free from obstacles.
-spec random_position() -> position().
random_position() ->
  gen_server:call(?SERVER, random_position).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_call({valid_moves, Position, Range}, _From, State) ->
  Reply = get_valid_moves(State, Position, Range),
  {reply, Reply, State};
handle_call({is_valid_position, Position}, _From, State) ->
  Reply = is_within_bounds(State, Position),
  {reply, Reply, State};
handle_call(random_position, _From, State) ->
  Reply = get_random_position(State),
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Get the positions in the world within range that are not
%% blocked by obstacles.
-spec get_valid_moves(state(), position(), pos_integer()) ->
                         [position()].
get_valid_moves(State, Position, Range) ->
  InRange = get_positions_in_range(State, Position, Range),
  FilterFun = fun(Pos) ->
                  not lists:member(Pos, State#state.obstacles)
              end,
  lists:filter(FilterFun, InRange).

%% @private
%% @doc Get a random position in the world without obstacles.
-spec get_random_position(state()) -> position().
get_random_position(#state{ dimension_x = DimX,
                            dimension_y = DimY,
                            obstacles   = Obstacles }) ->
  PossiblePositions = [ {X, Y} || X <- lists:seq(1, DimX),
                                  Y <- lists:seq(1, DimY) ],
  FreePositions = PossiblePositions -- Obstacles,
  lists:nth(random:uniform(length(FreePositions)), FreePositions).

%% @private
%% @doc Get the positions in the world within range.
-spec get_positions_in_range(state(), position(), pos_integer()) ->
                                [position()].
get_positions_in_range(State, Position, Range) ->
  Hypothetical = get_hypothetical_positions(Position, Range),
  FilterFun = fun(Pos) ->
                  is_within_bounds(State, Pos)
              end,
  lists:filter(FilterFun, Hypothetical).

%% @private
%% @doc Get positions within range.
%% This does not take into consideration the size of the world.
-spec get_hypothetical_positions(position(), pos_integer()) ->
                                    [position()].
get_hypothetical_positions({PosX, PosY}, Range) ->
  RelativeCoordinates = lists:seq(-Range, Range),
  [ {PosX + X, PosY + Y} || X <- RelativeCoordinates,
                            Y <- RelativeCoordinates,
                            not (X =:= 0 andalso Y =:= 0) ].

-spec is_within_bounds(state(), position()) -> boolean().
is_within_bounds(#state{ dimension_x = DimX, dimension_y = DimY }, {X, Y}) ->
  X > 0 andalso
    Y > 0 andalso
    X =< DimX andalso
    Y =< DimY.


%%%===================================================================
%%% EUnit tests
%%%===================================================================

-ifdef(TEST).

get_valid_moves_test_() ->
  State = #state{ obstacles = [{1, 0}],
                  dimension_x = 2,
                  dimension_y = 2
                },
  Position = {0, 0},
  ExpectedWithin1 = [{1, 1}
                    ],
  ExpectedWithin3 = [{1, 1},
                     {1, 2},
                     {2, 1},
                     {2, 2}
                    ],
  [ ?_assertEqual(ExpectedWithin1,
                  get_valid_moves(State, Position, 1)),
    %% make sure we can't move outside the world
    ?_assertEqual(ExpectedWithin3,
                  get_valid_moves(State, Position, 3))
  ].

get_hypothetical_positions_test() ->
  ?assertEqual([{0,0},
                {0,1},
                {0,2},
                {1,0},
                {1,2},
                {2,0},
                {2,1},
                {2,2}],
               get_hypothetical_positions({1, 1}, 1)).

is_within_bounds_test_() ->
  State = #state{ dimension_x = 1,
                  dimension_y = 1 },
  [ ?_assertEqual(true,  is_within_bounds(State, {1, 1})),
    ?_assertEqual(false, is_within_bounds(State, {0, 0})),
    ?_assertEqual(false, is_within_bounds(State, {1, 0})),
    ?_assertEqual(false, is_within_bounds(State, {2, 2})),
    ?_assertEqual(false, is_within_bounds(State, {-1, 1}))
  ].

get_random_position_setup() ->
  meck:new(random, [unstick, passthrough]),
  meck:expect(random, uniform, fun(X) -> X end),
  [random].

get_random_position_cleanup(MeckedModules) ->
  [meck:unload(Module) || Module <- MeckedModules].

get_random_position_test_() ->
  {setup, fun get_random_position_setup/0, fun get_random_position_cleanup/1,
   [?_test(
       begin
         State = #state{ dimension_x = 2,
                         dimension_y = 2,
                         obstacles = [{1, 2}, {2, 2}] },
         ?assertEqual({2, 1}, get_random_position(State))
       end)
    ]}.

-endif.
