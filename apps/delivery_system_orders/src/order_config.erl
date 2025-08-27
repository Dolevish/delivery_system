%%%-------------------------------------------------------------------
%% @doc order_config - מודול לניהול קונפיגורציה גמישה
%%      מנהל הגדרות כמו מרווחי זמן, טווחי מיקומים, סוגי עסקים
%%      וטעינה מקבצי קונפיגורציה או ארגומנטים של המערכת
%% @end
%%%-------------------------------------------------------------------
-module(order_config).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([get_generation_interval/0, set_generation_interval/1,
         get_location_range/0, set_location_range/2,
         get_business_types/0, add_business_type/2,
         get_config/1, set_config/2,
         load_config_file/1, save_config_file/1,
         reload_config/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-include_lib("kernel/include/file.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_CONFIG_FILE, "config/orders.config").

-record(state, {
    config = #{} :: map(),
    config_file :: string(),
    last_reload :: integer()
}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% קבלת מרווח זמן בין הזמנות
get_generation_interval() ->
    gen_server:call(?SERVER, get_generation_interval).

%% הגדרת מרווח זמן בין הזמנות
set_generation_interval(IntervalMs) ->
    gen_server:call(?SERVER, {set_generation_interval, IntervalMs}).

%% קבלת טווח מיקומים
get_location_range() ->
    gen_server:call(?SERVER, get_location_range).

%% הגדרת טווח מיקומים
set_location_range(MinLocation, MaxLocation) ->
    gen_server:call(?SERVER, {set_location_range, MinLocation, MaxLocation}).

%% קבלת סוגי עסקים
get_business_types() ->
    gen_server:call(?SERVER, get_business_types).

%% הוספת סוג עסק חדש
add_business_type(Type, Location) ->
    gen_server:call(?SERVER, {add_business_type, Type, Location}).

%% קבלת ערך קונפיגורציה כללי
get_config(Key) ->
    gen_server:call(?SERVER, {get_config, Key}).

%% הגדרת ערך קונפיגורציה
set_config(Key, Value) ->
    gen_server:call(?SERVER, {set_config, Key, Value}).

%% טעינת קובץ קונפיגורציה
load_config_file(FilePath) ->
    gen_server:call(?SERVER, {load_file, FilePath}).

%% שמירת קונפיגורציה לקובץ
save_config_file(FilePath) ->
    gen_server:call(?SERVER, {save_file, FilePath}).

%% טעינה מחדש של הקונפיגורציה
reload_config() ->
    gen_server:cast(?SERVER, reload).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %% טעינת קונפיגורציה מקובץ או מערכת
    ConfigFile = get_config_file_path(),
    Config = load_initial_config(ConfigFile),
    
    %% הפעלת טיימר לטעינה מחדש אוטומטית
    erlang:send_after(60000, self(), check_reload), % כל דקה
    
    io:format("Order configuration loaded from ~s.~n", [ConfigFile]),
    
    {ok, #state{
        config = Config,
        config_file = ConfigFile,
        last_reload = erlang:system_time(second)
    }}.

handle_call(get_generation_interval, _From, State) ->
    Interval = maps:get(generation_interval_ms, State#state.config, 5000),
    {reply, Interval, State};

handle_call({set_generation_interval, IntervalMs}, _From, State) ->
    NewConfig = maps:put(generation_interval_ms, IntervalMs, State#state.config),
    
    %% הודעה למחולל ההזמנות על השינוי
    notify_order_generator(interval_changed, IntervalMs),
    
    {reply, ok, State#state{config = NewConfig}};

handle_call(get_location_range, _From, State) ->
    MinLoc = maps:get(min_location, State#state.config, {0, 0}),
    MaxLoc = maps:get(max_location, State#state.config, {100, 100}),
    {reply, {MinLoc, MaxLoc}, State};

handle_call({set_location_range, MinLocation, MaxLocation}, _From, State) ->
    Config = State#state.config,
    NewConfig = maps:put(max_location, MaxLocation, maps:put(min_location, MinLocation, Config)),
    
    %% עדכון הולידטור
    order_validator:set_validation_rules(#{
        min_location => MinLocation,
        max_location => MaxLocation
    }),
    
    {reply, ok, State#state{config = NewConfig}};

handle_call(get_business_types, _From, State) ->
    BusinessTypes = maps:get(business_types, State#state.config, #{}),
    {reply, BusinessTypes, State};

handle_call({add_business_type, Type, Location}, _From, State) ->
    BusinessTypes = maps:get(business_types, State#state.config, #{}),
    UpdatedTypes = maps:put(Type, Location, BusinessTypes),
    NewConfig = maps:put(business_types, UpdatedTypes, State#state.config),
    
    io:format("Added business type ~p at location ~p.~n", [Type, Location]),
    
    {reply, ok, State#state{config = NewConfig}};

handle_call({get_config, Key}, _From, State) ->
    Value = maps:get(Key, State#state.config, undefined),
    {reply, Value, State};

handle_call({set_config, Key, Value}, _From, State) ->
    NewConfig = maps:put(Key, Value, State#state.config),
    
    %% הודעה למודולים רלוונטיים על שינוי
    notify_config_change(Key, Value),
    
    {reply, ok, State#state{config = NewConfig}};

handle_call({load_file, FilePath}, _From, State) ->
    case load_config_from_file(FilePath) of
        {ok, Config} ->
            %% החלת הקונפיגורציה החדשה
            apply_new_config(Config),
            
            {reply, ok, State#state{
                config = Config,
                config_file = FilePath,
                last_reload = erlang:system_time(second)
            }};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({save_file, FilePath}, _From, State) ->
    Result = save_config_to_file(FilePath, State#state.config),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(reload, State) ->
    %% טעינה מחדש מהקובץ הנוכחי
    case load_config_from_file(State#state.config_file) of
        {ok, Config} ->
            apply_new_config(Config),
            io:format("Configuration reloaded from ~s.~n", [State#state.config_file]),
            {noreply, State#state{
                config = Config,
                last_reload = erlang:system_time(second)
            }};
        {error, Reason} ->
            io:format("Failed to reload config: ~p~n", [Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(check_reload, State) ->
    %% בדיקה אם הקובץ השתנה
    case should_reload(State#state.config_file, State#state.last_reload) of
        true ->
            gen_server:cast(self(), reload);
        false ->
            ok
    end,
    
    %% תזמון הבדיקה הבאה
    erlang:send_after(60000, self(), check_reload),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% שמירת הקונפיגורציה הנוכחית
    save_config_to_file(State#state.config_file, State#state.config),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% פונקציות עזר פנימיות
%% ===================================================================

%% קבלת נתיב קובץ קונפיגורציה
get_config_file_path() ->
    %% בדיקה בארגומנטים של המערכת
    case application:get_env(delivery_system_orders, config_file) of
        {ok, Path} -> Path;
        undefined -> ?DEFAULT_CONFIG_FILE
    end.

%% טעינת קונפיגורציה התחלתית
load_initial_config(ConfigFile) ->
    %% ניסיון לטעון מקובץ
    case load_config_from_file(ConfigFile) of
        {ok, Config} ->
            Config;
        {error, _} ->
            %% יצירת קונפיגורציה ברירת מחדל
            create_default_config()
    end.

%% יצירת קונפיגורציה ברירת מחדל
create_default_config() ->
    #{
        %% מרווח זמן בין הזמנות (במילישניות)
        generation_interval_ms => 5000,
        
        %% טווח מיקומים
        min_location => {0, 0},
        max_location => {100, 100},
        
        %% סוגי עסקים וממיקומיהם
        business_types => #{
            restaurant => {25, 25},
            pharmacy => {75, 75},
            grocery => {50, 50},
            electronics => {30, 70},
            clothing => {70, 30}
        },
        
        %% הגדרות נוספות
        max_concurrent_orders => 100,
        order_timeout_ms => 600000,  % 10 דקות
        retry_failed_orders => true,
        max_retry_attempts => 3,
        
        %% הגדרות סימולציה
        simulation_speed => 1.0,  % מכפיל מהירות
        random_delays => true,
        failure_probability => 0.02,  % 2% סיכוי לכישלון
        
        %% הגדרות לקוחות
        customer_distribution => #{
            residential => 0.7,  % 70% באזורי מגורים
            commercial => 0.2,   % 20% באזורי מסחר
            industrial => 0.1    % 10% באזורי תעשייה
        },
        
        %% משקלים לבחירת עסקים
        business_weights => #{
            restaurant => 0.4,    % 40% מההזמנות
            pharmacy => 0.2,      % 20%
            grocery => 0.25,      % 25%
            electronics => 0.1,   % 10%
            clothing => 0.05      % 5%
        }
    }.

%% טעינת קונפיגורציה מקובץ
load_config_from_file(FilePath) ->
    case file:consult(FilePath) of
        {ok, [Config]} when is_map(Config) ->
            {ok, Config};
        {ok, Terms} ->
            %% המרת רשימת tuples למפה
            Config = lists:foldl(fun({Key, Value}, Acc) ->
                maps:put(Key, Value, Acc)
            end, #{}, Terms),
            {ok, Config};
        {error, enoent} ->
            {error, file_not_found};
        {error, Reason} ->
            {error, Reason}
    end.

%% שמירת קונפיגורציה לקובץ
save_config_to_file(FilePath, Config) ->
    %% הכנת המידע לכתיבה
    ConfigTerms = maps:to_list(Config),
    Content = lists:map(fun({Key, Value}) ->
        io_lib:format("{~p, ~p}.~n", [Key, Value])
    end, ConfigTerms),
    
    %% יצירת התיקייה אם לא קיימת
    filelib:ensure_dir(FilePath),
    
    %% כתיבה לקובץ
    case file:write_file(FilePath, Content) of
        ok ->
            io:format("Configuration saved to ~s.~n", [FilePath]),
            ok;
        {error, Reason} ->
            io:format("Failed to save config: ~p~n", [Reason]),
            {error, Reason}
    end.

%% בדיקה אם צריך לטעון מחדש
should_reload(FilePath, LastReload) ->
    case file:read_file_info(FilePath) of
        {ok, #file_info{mtime = MTime}} ->
            %% המרת זמן השינוי לשניות
            ModTimeSeconds = calendar:datetime_to_gregorian_seconds(MTime) 
                           - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
            ModTimeSeconds > LastReload;
        _ ->
            false
    end.

%% החלת קונפיגורציה חדשה
apply_new_config(Config) ->
    %% עדכון מרווח הזמן במחולל
    case maps:get(generation_interval_ms, Config, undefined) of
        undefined -> ok;
        Interval -> notify_order_generator(interval_changed, Interval)
    end,
    
    %% עדכון טווח מיקומים בולידטור
    MinLoc = maps:get(min_location, Config, {0, 0}),
    MaxLoc = maps:get(max_location, Config, {100, 100}),
    order_validator:set_validation_rules(#{
        min_location => MinLoc,
        max_location => MaxLoc
    }),
    
    io:format("New configuration applied successfully.~n").

%% הודעה למחולל ההזמנות על שינוי
notify_order_generator(interval_changed, NewInterval) ->
    %% שליחת הודעה למחולל (אם קיים)
    case whereis(order_generator) of
        undefined -> ok;
        Pid -> Pid ! {config_update, interval, NewInterval}
    end.

%% הודעה על שינוי קונפיגורציה
notify_config_change(Key, Value) ->
    %% הודעה למודולים רלוונטיים לפי המפתח
    case Key of
        generation_interval_ms ->
            notify_order_generator(interval_changed, Value);
        max_concurrent_orders ->
            %% הודעה למנהל השליחים
            courier_manager ! {config_update, max_orders, Value};
        _ ->
            ok
    end.