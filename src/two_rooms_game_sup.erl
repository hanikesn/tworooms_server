-module(two_rooms_game_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_all, 5, 10}, [
    {client_1,
      {two_rooms_client, start_link, [client1, [acceptor1]]},
      permanent, 1000, worker, []
    },
    {client_2,
      {two_rooms_client, start_link, [client2, [acceptor2]]},
      permanent, 1000, worker, []
    },
    {server,
      {two_rooms_server, start_link, []},
      permanent, 1000, worker, []
    }
  ]} }.

