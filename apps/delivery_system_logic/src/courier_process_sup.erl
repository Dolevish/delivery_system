%%%-------------------------------------------------------------------
%% @doc supervisor for managing individual courier processes.

%% @end
%%%-------------------------------------------------------------------
-module(courier_process_sup).
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
    % child specifications
    ChildTemplate = #{
        id => courier_process, % generic identifier
        start => {courier_process, start_link, []}, 
        restart => transient % restart only if the crash was not normal
    },

    {ok, {SupFlags, [ChildTemplate]}}.