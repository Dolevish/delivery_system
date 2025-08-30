%%%-------------------------------------------------------------------
%% @doc delivery_system_ui top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(delivery_system_ui_sup).

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
        % config_manager - manages system configuration
        #{
            id => config_manager,
            start => {config_manager, start_link, []},
            restart => permanent,
            type => worker
        },
        % graphics_server responsible for managing the display window.
        #{
            id => graphics_server,
            start => {graphics_server, start_link, []},
            restart => permanent,
            type => worker
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.