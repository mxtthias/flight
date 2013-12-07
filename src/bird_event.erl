-module(bird_event).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/0, delete_handler/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

start_link() ->
  gen_event:start_link().

add_handler() ->
  bird_event_manager:add_handler({?MODULE, self()}, []).

delete_handler() ->
  bird_event_manager:delete_handler({?MODULE, self()}, []).


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

handle_event({move, {Id, From, To}}, State) ->
  error_logger:info_msg("~p moved from ~p to ~p\n", [Id, From, To]),
  {ok, State};
handle_event(_Event, State) ->
  {ok, State}.

handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

handle_info(_Info, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

