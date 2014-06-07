-module(two_rooms_client).
-author("steffen").

-behaviour(gen_fsm).

%% API
-export([start_link/2, wait_for_start/2, round_running/2, wait_start_confirm_self/2, wait_start_confirm_other/2]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).


-record(state, {acceptor, state, server}).

start_link(Name, Args) ->
  gen_fsm:start_link({local, Name}, ?MODULE, Args, []).

init([Acceptor]) ->
  {ok, wait_for_start, #state{acceptor = Acceptor, server = two_rooms_server}}.

wait_for_start(cl_up, State) ->
  send_to_server(State, wait),
  {next_state, wait_start_confirm_self, State};
wait_for_start(wait, State) ->
  {next_state, wait_start_confirm_other, State};
wait_for_start(abort, State) ->
  {next_state, wait_for_start, State}.

wait_start_confirm_self(cl_down, State) ->
  send_to_server(State, abort),
  {next_state, wait_for_start, State};
wait_start_confirm_self(wait, State) ->
  {next_state, wait_start_confirm_self, State};
wait_start_confirm_self(start, State) ->
  send_to_client(State, start),
  {next_state, round_running, State}.

wait_start_confirm_other(cl_up, State) ->
  send_to_server(State, start),
  {next_state, wait_start_confirm_other, State};
wait_start_confirm_other(abort, State) ->
  {next_state, wait_for_start, State};
wait_start_confirm_other(start, State) ->
  send_to_client(State, start),
  {next_state, round_running, State}.

round_running(cl_down, State) ->
  {next_state, round_running, State};
round_running(cl_up, State) ->
  {next_state, round_running, State};
round_running(round_end, State) ->
  send_to_client(State, round_end),
  {next_state, wait_for_start, State};
round_running(game_end, State) ->
  send_to_client(State, game_end),
  {next_state, wait_for_start, State}.

handle_event(reset, _, State) ->
  send_to_server_all(State, reset),
  {next_state, wait_for_start, State};
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

send_to_client(#state{acceptor = Acceptor}, Msg) ->
  gen_server:cast(Acceptor, {send, Msg}).

send_to_server(#state{server = Server}, Msg) ->
  gen_fsm:send_event(Server, Msg).

send_to_server_all(#state{server = Server}, Msg) ->
  gen_fsm:send_all_state_event(Server, Msg).

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
