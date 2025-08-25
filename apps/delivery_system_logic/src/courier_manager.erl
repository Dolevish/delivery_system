%%%-------------------------------------------------------------------
%% @doc manager - manages the list of available and busy couriers.
%%      currently this is an empty skeleton.
%% @end
%%%-------------------------------------------------------------------
-module(courier_manager).

-export([start_link/0]).


start_link() ->
    {ok, self()}.