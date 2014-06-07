%%%-------------------------------------------------------------------
%%% @author steffen
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Mai 2014 01:56
%%%-------------------------------------------------------------------
-module(two_rooms_server).
-author("steffen").

-behaviour(gen_fsm).

%% API
-export([start_link/0, wait_for_start/2, wait_start_confirm/2, wait_for_next_game_buddy/2, wait_for_next_game/2, round_running/2]).

%% gen_fsm callbacks
-export([init/1,
  handle_event/3,
  handle_sync_event/4,
  handle_info/3,
  terminate/3,
  code_change/4]).

-define(SERVER, ?MODULE).

-define(ROUNDS, [5,4,3,2,1]).

-record(state, {rounds=?ROUNDS, client1, client2}).

% wait_for_start
% wait_for_buddy_start
% wait_for_running
% wait_for_next_game


-spec(start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  {ok, wait_for_start, #state{rounds = ?ROUNDS, client1 = client1, client2 = client2}}.


wait_for_start(wait, State) ->
  send_to_clients(State, wait),
  {next_state, wait_start_confirm, State}.

wait_start_confirm(abort, State) ->
  send_to_clients(State, abort),
  {next_state, wait_for_start, State};

wait_start_confirm(start, #state{rounds = [Time | Rest]} = State) ->
  send_to_clients(State, start),
  gen_fsm:start_timer(Time * 1000, round_end),
  {next_state, round_running, State#state{rounds = Rest}}.

round_running({timeout,_,round_end}, #state{rounds = []} = State) ->
  send_to_clients(State, game_end),
  {next_state, wait_for_next_game, State};
round_running({timeout,_,round_end}, State) ->
  send_to_clients(State, round_end),
  {next_state, wait_for_start, State};
round_running(Msg, State) ->
  io:format("RR ~p\n", [Msg]),
  {next_state, round_running, State}.

wait_for_next_game(next, State) ->
  {next_state, wait_for_next_game_buddy, State}.

wait_for_next_game_buddy(next, State) ->
  {next_state, wait_for_start, State#state{rounds = ?ROUNDS}}.


handle_event(reset, _, State) ->
  io:format("Reset Game"),
  send_to_clients_all(State, reset),
  {next_state, wait_for_start, State#state{rounds = ?ROUNDS}};
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

send_to_clients(#state{client1 = Client1, client2 = Client2}, Msg) ->
  gen_fsm:send_event(Client1, Msg),
  gen_fsm:send_event(Client2, Msg).

send_to_clients_all(#state{client1 = Client1, client2 = Client2}, Msg) ->
  gen_fsm:send_all_state_event(Client1, Msg),
  gen_fsm:send_all_state_event(Client2, Msg).

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
