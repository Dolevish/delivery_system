%%%-------------------------------------------------------------------
%% Order_process_sup - dynamic supervisor for managing individual order processes.
%%%-------------------------------------------------------------------
-module(order_process_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Supervisor flags
    SupFlags = #{
        strategy => simple_one_for_one
    },

    % Child template for dynamically created order processes
    ChildTemplate = #{
        id => order_process, % Generic identifier
        start => {order_process, start_link, []}, 
        restart => transient % Restart only if the crash was not normal
    },

    {ok, {SupFlags, [ChildTemplate]}}.

