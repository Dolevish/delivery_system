%%%-------------------------------------------------------------------
%% @doc delivery_system_logic public API
%% @end
%%%-------------------------------------------------------------------

-module(delivery_system_logic_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    delivery_system_logic_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
