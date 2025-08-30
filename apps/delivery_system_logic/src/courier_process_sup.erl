%%%-------------------------------------------------------------------
%% @doc supervisor for managing individual courier processes.
%%      Uses simple_one_for_one strategy for dynamic child creation
%% @end
%%%-------------------------------------------------------------------
-module(courier_process_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(_InitialCouriers) ->
    %% Ignore initial couriers - will be added dynamically
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 5,
        period => 10     
    },

    %% Child template for dynamic courier processes
    ChildSpec = #{
        id => courier_process,
        start => {courier_process, start_link, []},
        restart => permanent,
        type => worker
    },

    {ok, {SupFlags, [ChildSpec]}}.