%%%-------------------------------------------------------------------
%% @doc graphics_server module. responsible for managing the display window.
%% @end
%%%-------------------------------------------------------------------
-module(graphics_server).

-export([start_link/0]).

start_link() ->
    {ok, self()}.