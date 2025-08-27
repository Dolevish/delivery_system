%%%-------------------------------------------------------------------
%% @doc supervisor for managing individual courier processes.
%%      FIXED: Now passes complete CourierData to each courier process
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
        strategy => one_for_one,
        intensity => 5,
        period => 10     
    },

 
    ChildSpecs = [
        #{
            id => maps:get(id, CourierData), %% Identifier for the courier process
            start => {courier_process, start_link, [CourierData]}, %% Passes all CourierData
            restart => permanent,
            type => worker
        }
        || CourierData <- InitialCouriers %% Iterate over each courier's data
    ],

    {ok, {SupFlags, ChildSpecs}}.