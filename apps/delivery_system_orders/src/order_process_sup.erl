%%%-------------------------------------------------------------------
%% @doc order_process_sup - dynamic supervisor for managing individual order processes.
%% @end
%%%-------------------------------------------------------------------
-module(order_process_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % supervisor flags
    SupFlags = #{
        strategy => simple_one_for_one
    },

    % child template for dynamically created order processes
    ChildTemplate = #{
        id => order_process, % generic identifier
        start => {order_process, start_link, []}, 
        restart => transient % restart only if the crash was not normal
    },

    {ok, {SupFlags, [ChildTemplate]}}.