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
    %supervisor flags
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },

    %child specifications
    ChildSpecs = [

        %manager of couriers
        #{
            id => courier_manager,
            start => {courier_manager, start_link, []},
            restart => permanent,
            type => worker
        },

        %dynamic supervisor - its role will be to manage all individual courier processes.
        #{
            id => courier_process_sup,
            start => {courier_process_sup, start_link, []},
            restart => permanent,
            type => supervisor
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.