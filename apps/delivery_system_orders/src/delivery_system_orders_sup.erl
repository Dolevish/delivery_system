%%%-------------------------------------------------------------------
%% @doc delivery_system_orders top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(delivery_system_orders_sup).

-behaviour(supervisor).

-export([start_link/0, start_order_process/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    % supervisor flags
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    % child specifications
    ChildSpecs = [
        % order_generator responsible for creating new orders
        #{
            id => order_generator,
            start => {order_generator, start_link, []},
            restart => permanent,
            type => worker
        },

        % dynamic supervisor - its role will be to manage all individual order processes.
        % every active order in the system will be a "child" of this supervisor.
        #{
            id => order_process_sup,
            start => {order_process_sup, start_link, []},
            restart => permanent,
            type => supervisor
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.


%% API function to dynamically start a new order_process child.
start_order_process(OrderData) ->
    ChildSpec = #{
        id => {order_process, maps:get(id, OrderData)}, % Unique ID for the child
        start => {order_process, start_link, [OrderData]},
        restart => transient
    },
    supervisor:start_child(?MODULE, ChildSpec).