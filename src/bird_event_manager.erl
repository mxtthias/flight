-module(bird_event_manager).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/2, delete_handler/2]).
-export([move/3]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
  gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
  gen_event:delete_handler(?SERVER, Handler, Args).

move(Pid, From, To) ->
  gen_event:notify(?SERVER, {move, Pid, From, To}).


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
  {ok, #state{}}.

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

