%%%-------------------------------------------------------------------
%%% @author steffen
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Mai 2014 01:02
%%%-------------------------------------------------------------------
-module(two_rooms).
-author("steffen").

%% API
-export([start/0]).

start() ->
  application:ensure_started(two_rooms).