-module(bird).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(SLEEP_TIME, 2000).

-record(state, { position,
                 direction,
                 sight = 2,
                 neighbors = [],
                 event_pid
               }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, Pid} = bird_event:start_link(),
  bird_event:add_handler(),
  erlang:send_after(random_delay(), self(), trigger),
  {ok, #state{ event_pid = Pid }}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(trigger, State) ->
  bird_event_manager:move(self(), from, to),
  erlang:send_after(random_delay(), self(), trigger),
  {noreply, State};
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
