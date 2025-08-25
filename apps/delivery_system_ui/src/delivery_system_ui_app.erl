%%%-------------------------------------------------------------------
%% @doc delivery_system_ui public API
%% @end
%%%-------------------------------------------------------------------

-module(delivery_system_ui_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    delivery_system_ui_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
