%%%-------------------------------------------------------------------
%% @doc delivery_system_logic top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(delivery_system_logic_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    % Supervisor flags
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    % Start with empty couriers list - will be initialized from GUI
    InitialCouriers = [],

    % Child specifications
    ChildSpecs = [

        % Manager of couriers
        #{
            id => courier_manager,
            start => {courier_manager, start_link, [InitialCouriers]},
            restart => permanent,
            type => worker
        },

        % Dynamic supervisor - its role will be to manage all individual courier processes.
        #{
            id => courier_process_sup,
            start => {courier_process_sup, start_link, [InitialCouriers]},
            restart => permanent,
            type => supervisor
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.