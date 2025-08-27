%%%-------------------------------------------------------------------
%% Delivery_system_orders top level supervisor.
%% עכשיו כולל את כל המודולים החדשים לניהול מתקדם של הזמנות
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
        strategy => one_for_all,  %% שינוי לone_for_all כי המודולים תלויים זה בזה
        intensity => 5,
        period => 10
    },

    %% child specifications - סדר חשוב! המודולים הבסיסיים קודם
    ChildSpecs = [
        %% מודול קונפיגורציה - חייב להיות ראשון
        #{
            id => order_config,
            start => {order_config, start_link, []},
            restart => permanent,
            type => worker,
            shutdown => 5000
        },

        %% מודול ולידציה - משתמש בקונפיגורציה
        #{
            id => order_validator,
            start => {order_validator, start_link, []},
            restart => permanent,
            type => worker,
            shutdown => 5000
        },

        %% מאגר נתוני הזמנות - מרכזי וקריטי
        #{
            id => order_repository,
            start => {order_repository, start_link, []},
            restart => permanent,
            type => worker,
            shutdown => 5000
        },

        %% מודול היסטוריה - תלוי ב-repository
        #{
            id => order_history,
            start => {order_history, start_link, []},
            restart => permanent,
            type => worker,
            shutdown => 5000
        },

        %% מודול אנליטיקה - תלוי ב-history
        #{
            id => order_analytics,
            start => {order_analytics, start_link, []},
            restart => permanent,
            type => worker,
            shutdown => 5000
        },

        %% API חיצוני - משתמש בכל המודולים הקודמים
        #{
            id => order_api,
            start => {order_api, start_link, []},
            restart => permanent,
            type => worker,
            shutdown => 5000
        },

        %% order_generator - המחולל האוטומטי, אחרון כי משתמש בכל השאר
        #{
            id => order_generator,
            start => {order_generator, start_link, []},
            restart => permanent,
            type => worker,
            shutdown => 5000
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% ===================================================================
%% הערות על סדר ההפעלה:
%% ===================================================================
%% 1. order_config - מספק קונפיגורציה לכל המודולים
%% 2. order_validator - משתמש בקונפיגורציה לולידציה
%% 3. order_repository - מאגר מרכזי לכל נתוני ההזמנות
%% 4. order_history - שומר היסטוריה של הזמנות שהושלמו
%% 5. order_analytics - מחשב סטטיסטיקות על בסיס ההיסטוריה
%% 6. order_api - מספק ממשק חיצוני לכל הפונקציונליות
%% 7. order_generator - מייצר הזמנות אוטומטיות
%%
%% השימוש ב-one_for_all מבטיח שאם מודול קריטי נופל,
%% כל המודולים יופעלו מחדש בסדר הנכון