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
  self() ! listen,
  {ok, #state{listensocket = ListenSocket, client = Client}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(listen, #state{listensocket = ListenSocket, client = Client} = State) ->
  {ok, Socket} = gen_tcp:accept(ListenSocket),
  io:format("Socket accepted for ~p\n", [Client]),
  {noreply, State#state{socket = Socket}};
handle_info({tcp_closed,_}, #state{client = Client} = State) ->
  io:format("Socket closed for ~p.\n", [Client]),
  self() ! listen,
  {noreply, State};
handle_info({tcp,_,Msg}, #state{client = Client} = State) ->
  Client ! Msg,
  {noreply, State}.

terminate(_Reason, #state{listensocket = ListenSocket}) ->
  gen_tcp:close(ListenSocket),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
