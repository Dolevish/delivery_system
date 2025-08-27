%%%-------------------------------------------------------------------
%% @doc order_analytics - מודול לחישוב סטטיסטיקות בזמן אמת
%%      מחשב מדדים כמו מספר הזמנות לשעה, זמן משלוח ממוצע,
%%      אזורים פופולריים, שליחים יעילים ושיעורי הצלחה
%% @end
%%%-------------------------------------------------------------------
-module(order_analytics).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([update_order_completed/1, update_order_failed/1,
         get_hourly_stats/0, get_daily_stats/0, 
         get_average_delivery_time/0, get_average_delivery_time/1,
         get_popular_locations/0, get_courier_stats/0,
         get_success_rate/0, get_realtime_dashboard/0,
         reset_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(stats, {
    orders_completed = 0 :: integer(),
    orders_failed = 0 :: integer(),
    total_delivery_time = 0 :: integer(),
    delivery_times = [] :: [integer()],
    hourly_counts = #{} :: #{integer() => integer()},
    daily_counts = #{} :: #{integer() => integer()},
    location_heat_map = #{} :: #{{integer(), integer()} => integer()},
    courier_performance = #{} :: #{atom() => map()},
    last_update = 0 :: integer()
}).

-record(state, {
    current_stats :: #stats{},
    historical_stats = [] :: [#stats{}],
    start_time :: integer()
}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% עדכון סטטיסטיקות עבור הזמנה שהושלמה
update_order_completed(OrderData) ->
    gen_server:cast(?SERVER, {order_completed, OrderData}).

%% עדכון סטטיסטיקות עבור הזמנה שנכשלה
update_order_failed(OrderData) ->
    gen_server:cast(?SERVER, {order_failed, OrderData}).

%% קבלת סטטיסטיקות לפי שעה
get_hourly_stats() ->
    gen_server:call(?SERVER, get_hourly_stats).

%% קבלת סטטיסטיקות יומיות
get_daily_stats() ->
    gen_server:call(?SERVER, get_daily_stats).

%% קבלת זמן משלוח ממוצע
get_average_delivery_time() ->
    gen_server:call(?SERVER, get_avg_delivery_time).

%% קבלת זמן משלוח ממוצע לשליח ספציפי
get_average_delivery_time(CourierID) ->
    gen_server:call(?SERVER, {get_avg_delivery_time, CourierID}).

%% קבלת מיקומים פופולריים
get_popular_locations() ->
    gen_server:call(?SERVER, get_popular_locations).

%% קבלת סטטיסטיקות שליחים
get_courier_stats() ->
    gen_server:call(?SERVER, get_courier_stats).

%% קבלת שיעור הצלחה
get_success_rate() ->
    gen_server:call(?SERVER, get_success_rate).

%% קבלת דשבורד מלא בזמן אמת
get_realtime_dashboard() ->
    gen_server:call(?SERVER, get_dashboard).

%% איפוס סטטיסטיקות
reset_stats() ->
    gen_server:cast(?SERVER, reset_stats).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %% הפעלת טיימר לארכיון סטטיסטיקות כל שעה
    erlang:send_after(3600000, self(), archive_stats),
    
    StartTime = erlang:system_time(millisecond),
    
    io:format("Order analytics engine started.~n"),
    
    {ok, #state{
        current_stats = #stats{last_update = StartTime},
        historical_stats = [],
        start_time = StartTime
    }}.

handle_call(get_hourly_stats, _From, State) ->
    HourlyStats = calculate_hourly_stats(State#state.current_stats),
    {reply, HourlyStats, State};

handle_call(get_daily_stats, _From, State) ->
    DailyStats = calculate_daily_stats(State#state.current_stats),
    {reply, DailyStats, State};

handle_call(get_avg_delivery_time, _From, State) ->
    AvgTime = calculate_average_delivery_time(State#state.current_stats),
    {reply, AvgTime, State};

handle_call({get_avg_delivery_time, CourierID}, _From, State) ->
    AvgTime = calculate_courier_avg_delivery_time(
        CourierID, 
        State#state.current_stats#stats.courier_performance
    ),
    {reply, AvgTime, State};

handle_call(get_popular_locations, _From, State) ->
    PopularLocs = analyze_popular_locations(
        State#state.current_stats#stats.location_heat_map
    ),
    {reply, PopularLocs, State};

handle_call(get_courier_stats, _From, State) ->
    CourierStats = format_courier_statistics(
        State#state.current_stats#stats.courier_performance
    ),
    {reply, CourierStats, State};

handle_call(get_success_rate, _From, State) ->
    Stats = State#state.current_stats,
    Total = Stats#stats.orders_completed + Stats#stats.orders_failed,
    Rate = case Total of
        0 -> 100.0;
        _ -> (Stats#stats.orders_completed / Total) * 100
    end,
    {reply, {Rate, Total}, State};

handle_call(get_dashboard, _From, State) ->
    Dashboard = create_dashboard(State),
    {reply, Dashboard, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({order_completed, OrderData}, State) ->
    NewStats = update_completed_stats(State#state.current_stats, OrderData),
    {noreply, State#state{current_stats = NewStats}};

handle_cast({order_failed, OrderData}, State) ->
    NewStats = update_failed_stats(State#state.current_stats, OrderData),
    {noreply, State#state{current_stats = NewStats}};

handle_cast(reset_stats, State) ->
    %% שמירת הסטטיסטיקות הנוכחיות בהיסטוריה לפני איפוס
    Historical = [State#state.current_stats | State#state.historical_stats],
    
    %% יצירת סטטיסטיקות חדשות
    NewStats = #stats{last_update = erlang:system_time(millisecond)},
    
    io:format("Analytics stats reset. Previous stats archived.~n"),
    
    {noreply, State#state{
        current_stats = NewStats,
        historical_stats = Historical
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(archive_stats, State) ->
    %% ארכיון סטטיסטיקות נוכחיות
    Historical = [State#state.current_stats | State#state.historical_stats],
    
    %% שמירת רק 24 שעות של היסטוריה
    TrimmedHistory = case length(Historical) > 24 of
        true -> lists:sublist(Historical, 24);
        false -> Historical
    end,
    
    %% תזמון הארכיון הבא
    erlang:send_after(3600000, self(), archive_stats),
    
    {noreply, State#state{historical_stats = TrimmedHistory}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% שמירת סטטיסטיקות לפני סגירה
    save_analytics_data(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% פונקציות חישוב וניתוח פנימיות
%% ===================================================================

%% עדכון סטטיסטיקות להזמנה שהושלמה
update_completed_stats(Stats, OrderData) ->
    %% חישוב זמן משלוח
    CreatedAt = maps:get(created_at, OrderData, 0),
    CompletedAt = maps:get(completed_at, OrderData, erlang:system_time(millisecond)),
    DeliveryTime = CompletedAt - CreatedAt,
    
    %% עדכון מונים
    NewCompleted = Stats#stats.orders_completed + 1,
    NewTotalTime = Stats#stats.total_delivery_time + DeliveryTime,
    NewDeliveryTimes = [DeliveryTime | Stats#stats.delivery_times],
    
    %% עדכון סטטיסטיקות לפי שעה
    Hour = get_current_hour(),
    HourlyMap = maps:update_with(Hour, fun(V) -> V + 1 end, 1, Stats#stats.hourly_counts),
    
    %% עדכון סטטיסטיקות לפי יום
    Day = get_current_day(),
    DailyMap = maps:update_with(Day, fun(V) -> V + 1 end, 1, Stats#stats.daily_counts),
    
    %% עדכון מפת חום של מיקומים
    CustomerLoc = maps:get(customer_location, OrderData),
    HeatMap = update_location_heat_map(Stats#stats.location_heat_map, CustomerLoc),
    
    %% עדכון ביצועי שליח
    CourierID = maps:get(assigned_courier, OrderData),
    CourierPerf = update_courier_performance(
        Stats#stats.courier_performance, 
        CourierID, 
        DeliveryTime, 
        completed
    ),
    
    Stats#stats{
        orders_completed = NewCompleted,
        total_delivery_time = NewTotalTime,
        delivery_times = keep_recent_times(NewDeliveryTimes, 1000),
        hourly_counts = HourlyMap,
        daily_counts = DailyMap,
        location_heat_map = HeatMap,
        courier_performance = CourierPerf,
        last_update = erlang:system_time(millisecond)
    }.

%% עדכון סטטיסטיקות להזמנה שנכשלה
update_failed_stats(Stats, OrderData) ->
    NewFailed = Stats#stats.orders_failed + 1,
    
    %% עדכון ביצועי שליח אם רלוונטי
    CourierPerf = case maps:get(assigned_courier, OrderData, undefined) of
        undefined -> Stats#stats.courier_performance;
        CourierID -> 
            update_courier_performance(
                Stats#stats.courier_performance, 
                CourierID, 
                0, 
                failed
            )
    end,
    
    Stats#stats{
        orders_failed = NewFailed,
        courier_performance = CourierPerf,
        last_update = erlang:system_time(millisecond)
    }.

%% עדכון מפת חום של מיקומים
update_location_heat_map(HeatMap, {X, Y}) ->
    %% עיגול לגריד של 10x10
    GridX = (X div 10) * 10,
    GridY = (Y div 10) * 10,
    maps:update_with({GridX, GridY}, fun(V) -> V + 1 end, 1, HeatMap).

%% עדכון ביצועי שליח
update_courier_performance(PerfMap, CourierID, DeliveryTime, Status) ->
    CourierStats = maps:get(CourierID, PerfMap, #{
        completed => 0,
        failed => 0,
        total_time => 0,
        avg_time => 0
    }),
    
    UpdatedStats = case Status of
        completed ->
            Completed = maps:get(completed, CourierStats, 0) + 1,
            TotalTime = maps:get(total_time, CourierStats, 0) + DeliveryTime,
            CourierStats#{
                completed => Completed,
                total_time => TotalTime,
                avg_time => TotalTime div Completed
            };
        failed ->
            CourierStats#{
                failed => maps:get(failed, CourierStats, 0) + 1
            }
    end,
    
    maps:put(CourierID, UpdatedStats, PerfMap).

%% חישוב סטטיסטיקות לפי שעה
calculate_hourly_stats(Stats) ->
    CurrentHour = get_current_hour(),
    Last24Hours = lists:seq(CurrentHour - 23, CurrentHour),
    
    lists:map(fun(Hour) ->
        Count = maps:get(Hour rem 24, Stats#stats.hourly_counts, 0),
        {Hour rem 24, Count}
    end, Last24Hours).

%% חישוב סטטיסטיקות יומיות
calculate_daily_stats(Stats) ->
    Stats#stats.daily_counts.

%% חישוב זמן משלוח ממוצע
calculate_average_delivery_time(Stats) ->
    case Stats#stats.orders_completed of
        0 -> 0;
        N -> Stats#stats.total_delivery_time div N
    end.

%% חישוב זמן משלוח ממוצע לשליח
calculate_courier_avg_delivery_time(CourierID, CourierPerf) ->
    case maps:get(CourierID, CourierPerf, undefined) of
        undefined -> 0;
        CourierStats -> maps:get(avg_time, CourierStats, 0)
    end.

%% ניתוח מיקומים פופולריים
analyze_popular_locations(HeatMap) ->
    %% מיון לפי פופולריות
    Sorted = lists:sort(fun({_, Count1}, {_, Count2}) ->
        Count1 >= Count2
    end, maps:to_list(HeatMap)),
    
    %% החזרת ה-10 הכי פופולריים
    lists:sublist(Sorted, 10).

%% פורמט סטטיסטיקות שליחים
format_courier_statistics(CourierPerf) ->
    maps:map(fun(CourierID, Stats) ->
        Completed = maps:get(completed, Stats, 0),
        Failed = maps:get(failed, Stats, 0),
        Total = Completed + Failed,
        SuccessRate = case Total of
            0 -> 100.0;
            _ -> (Completed / Total) * 100
        end,
        
        #{
            courier_id => CourierID,
            completed => Completed,
            failed => Failed,
            success_rate => SuccessRate,
            avg_delivery_time => maps:get(avg_time, Stats, 0)
        }
    end, CourierPerf).

%% יצירת דשבורד מלא
create_dashboard(State) ->
    Stats = State#state.current_stats,
    Uptime = erlang:system_time(millisecond) - State#state.start_time,
    
    #{
        uptime_ms => Uptime,
        orders => #{
            completed => Stats#stats.orders_completed,
            failed => Stats#stats.orders_failed,
            success_rate => calculate_success_rate(Stats)
        },
        delivery => #{
            avg_time_ms => calculate_average_delivery_time(Stats),
            median_time_ms => calculate_median_time(Stats#stats.delivery_times),
            min_time_ms => calculate_min_time(Stats#stats.delivery_times),
            max_time_ms => calculate_max_time(Stats#stats.delivery_times)
        },
        hourly_trend => calculate_hourly_stats(Stats),
        top_locations => analyze_popular_locations(Stats#stats.location_heat_map),
        courier_rankings => rank_couriers(Stats#stats.courier_performance),
        last_update => Stats#stats.last_update
    }.

%% חישוב שיעור הצלחה
calculate_success_rate(Stats) ->
    Total = Stats#stats.orders_completed + Stats#stats.orders_failed,
    case Total of
        0 -> 100.0;
        _ -> (Stats#stats.orders_completed / Total) * 100
    end.

%% חישוב זמן חציוני
calculate_median_time([]) -> 0;
calculate_median_time(Times) ->
    Sorted = lists:sort(Times),
    Length = length(Sorted),
    case Length rem 2 of
        0 ->
            Mid1 = lists:nth(Length div 2, Sorted),
            Mid2 = lists:nth((Length div 2) + 1, Sorted),
            (Mid1 + Mid2) div 2;
        1 ->
            lists:nth((Length div 2) + 1, Sorted)
    end.

%% חישוב זמן מינימלי
calculate_min_time([]) -> 0;
calculate_min_time(Times) -> lists:min(Times).

%% חישוב זמן מקסימלי
calculate_max_time([]) -> 0;
calculate_max_time(Times) -> lists:max(Times).

%% דירוג שליחים
rank_couriers(CourierPerf) ->
    CourierList = maps:to_list(CourierPerf),
    
    %% מיון לפי שיעור הצלחה וזמן ממוצע
    Sorted = lists:sort(fun({_, Stats1}, {_, Stats2}) ->
        Score1 = calculate_courier_score(Stats1),
        Score2 = calculate_courier_score(Stats2),
        Score1 >= Score2
    end, CourierList),
    
    %% החזרת ה-5 הטובים
    lists:sublist(Sorted, 5).

%% חישוב ציון שליח
calculate_courier_score(Stats) ->
    Completed = maps:get(completed, Stats, 0),
    Failed = maps:get(failed, Stats, 0),
    AvgTime = maps:get(avg_time, Stats, 999999),
    
    %% ציון משוקלל: 70% שיעור הצלחה, 30% מהירות
    SuccessScore = case Completed + Failed of
        0 -> 0;
        Total -> (Completed / Total) * 70
    end,
    
    %% נרמול זמן (פחות זמן = יותר נקודות)
    SpeedScore = case AvgTime of
        0 -> 0;
        T when T < 60000 -> 30;  % מתחת לדקה
        T when T < 300000 -> 20; % מתחת ל-5 דקות
        _ -> 10
    end,
    
    SuccessScore + SpeedScore.

%% שמירת רק הזמנים האחרונים
keep_recent_times(Times, MaxCount) when length(Times) > MaxCount ->
    lists:sublist(Times, MaxCount);
keep_recent_times(Times, _) ->
    Times.

%% קבלת השעה הנוכחית
get_current_hour() ->
    {_, {Hour, _, _}} = calendar:now_to_datetime(erlang:timestamp()),
    Hour.

%% קבלת היום הנוכחי
get_current_day() ->
    {Date, _} = calendar:now_to_datetime(erlang:timestamp()),
    calendar:date_to_gregorian_days(Date).

%% שמירת נתוני אנליטיקה
save_analytics_data(_State) ->
    %% כאן ניתן לשמור לקובץ או DB
    io:format("Saving analytics data...~n").