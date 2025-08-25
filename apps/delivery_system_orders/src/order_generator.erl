%%%-------------------------------------------------------------------
%% @doc order_generator - responsible for creating new orders in the system.
%% @end
%%%-------------------------------------------------------------------
-module(order_generator).

-export([start_link/0]).

start_link() ->
    {ok, self()}.