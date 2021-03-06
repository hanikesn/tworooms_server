-module(two_rooms_sup).

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
    {ok, { {one_for_one, 5, 10}, [
      {acceptor_1,
        {two_rooms_acceptor, start_link, [acceptor1, [3000, client1]]},
        permanent, 1000, worker, []
      },
      {acceptor_2,
        {two_rooms_acceptor, start_link, [acceptor2,[3001, client2]]},
        permanent, 1000, worker, []
      },
      {game,
        {two_rooms_game_sup, start_link, []},
        permanent, 1000, supervisor, []
      }
    ]} }.

