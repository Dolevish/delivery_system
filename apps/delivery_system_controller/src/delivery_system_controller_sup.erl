%%%-------------------------------------------------------------------
%% @doc delivery_system top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(delivery_system_controller_sup).

-behaviour(supervisor).

-export([start_link/0]).

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

        %% local processes
        #{
            id => cluster_monitor,
            start => {cluster_monitor, start_link, []},
            restart => permanent,
            type => worker
        },
        #{
            id => db_worker,
            start => {db_worker, start_link, []},
            restart => permanent,
            type => worker
        }
    ],




                   
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
