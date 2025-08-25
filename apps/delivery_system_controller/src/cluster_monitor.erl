%%%-------------------------------------------------------------------
%% @doc monitors the health of nodes in the cluster.
%% @end
%%%-------------------------------------------------------------------
-module(cluster_monitor).

-export([start_link/0]).


start_link() ->
    %% Currently returning a generic response to keep the supervision working.
    %% Later we will replace this with real gen_server code.
    {ok, self()}.