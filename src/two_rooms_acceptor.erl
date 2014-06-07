-module(two_rooms_acceptor).
-author("steffen").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {listensocket, socket, client}).

start_link(Name, Args) ->
  gen_server:start_link({local, Name}, ?MODULE, Args, []).

init([Port, Client]) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
  io:format("Socket open for ~p on ~p\n", [Client, Port]),
  self() ! accept,
  {ok, #state{listensocket = ListenSocket, client = Client}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({send, Msg}, #state{socket = Socket} = State) when is_atom(Msg) ->
  gen_tcp:send(Socket, [atom_to_binary(Msg, latin1), <<"\n">>]),
  {noreply, State}.

handle_info(accept, #state{listensocket = ListenSocket, client = Client} = State) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  io:format("Socket accepted for ~p\n", [Client]),
  {noreply, State#state{socket = Socket}};

handle_info({tcp_closed,_}, #state{client = Client} = State) ->
  io:format("Socket closed for ~p.\n", [Client]),
  self() ! accept,
  {noreply, State};

handle_info({tcp,_,<<"reset">>}, #state{client = Client} = State) ->
  io:format("Client Reset.\n"),
  gen_fsm:send_all_state_event(Client, reset),
  {noreply, State};
handle_info({tcp,_,Msg}, #state{client = Client} = State) ->
  io:format("Client Msg: ~p\n", [Msg]),
  case translate(Msg) of
    undefined -> ok;
    M ->gen_fsm:send_event(Client, M)
  end,
  {noreply, State}.

terminate(_Reason, #state{listensocket = ListenSocket}) ->
  gen_tcp:close(ListenSocket),
  ok.

translate(<<"reset\n">>) ->
  cl_reset;
translate(<<"down\n">>) ->
  cl_up;
translate(<<"up\n">>) ->
  cl_down;
translate(_) ->
  undefined.




code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
