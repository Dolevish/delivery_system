%%%-------------------------------------------------------------------
%% Delivery_system_orders top level supervisor.
%% 
%%%-------------------------------------------------------------------

-module(delivery_system_orders_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    %% supervisor flags
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    %% child specifications
    ChildSpecs = [
        %% order_generator responsible for creating new orders
        #{
            id => order_generator,
            start => {order_generator, start_link, []},
            restart => permanent,
            type => worker
        }

       
    ],

    {ok, {SupFlags, ChildSpecs}}.