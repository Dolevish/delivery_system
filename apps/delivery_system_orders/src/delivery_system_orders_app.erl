%%%-------------------------------------------------------------------
%% @doc delivery_system_orders public API
%% @end
%%%-------------------------------------------------------------------

-module(delivery_system_orders_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    delivery_system_orders_sup:start_link().

stop(_State) ->
    ok.
