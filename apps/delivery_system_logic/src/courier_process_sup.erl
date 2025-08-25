%%%-------------------------------------------------------------------
%% @doc supervisor for managing individual courier processes.
%% @end
%%%-------------------------------------------------------------------
-module(courier_process_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(InitialCouriers) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [InitialCouriers]).

init([InitialCouriers]) ->
    SupFlags = #{
        strategy => one_for_one
    },

    ChildSpecs = [
        #{
            id => maps:get(id, CourierData), %% Identifier for the courier process
            start => {courier_process, start_link, [maps:get(id, CourierData)]}, %% Pass the courier ID to the process
            restart => permanent,
            type => worker
        }
        || CourierData <- InitialCouriers %% Iterate over each courier's data
    ],

    {ok, {SupFlags, ChildSpecs}}.