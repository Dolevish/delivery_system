%%%-------------------------------------------------------------------
%% @doc order_history - מודול לאחסון והיסטוריה של הזמנות שהושלמו
%%      מאחסן במבנה נתונים מתאים את כל ההזמנות שהושלמו
%%      מאפשר שאילתות מתקדמות על ההיסטוריה
%% @end
%%%-------------------------------------------------------------------
-module(order_history).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_completed_order/1, get_order_history/1, 
         get_orders_by_date_range/2, get_orders_by_courier/1,
         get_orders_by_location/2, search_orders/1,
         export_history/1, clear_old_history/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(HISTORY_RETENTION_DAYS, 90). %% שמירת היסטוריה ל-90 יום

-record(state, {
    history_tid,      %% ETS table להיסטוריה
    index_by_date,    %% אינדקס לפי תאריך
    index_by_courier, %% אינדקס לפי שליח
    index_by_location,%% אינדקס לפי מיקום
    total_orders      %% סך כל ההזמנות בהיסטוריה
}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% הוספת הזמנה שהושלמה להיסטוריה
add_completed_order(OrderData) ->
    gen_server:cast(?SERVER, {add_order, OrderData}).

%% קבלת היסטוריה של הזמנה ספציפית
get_order_history(OrderID) ->
    gen_server:call(?SERVER, {get_order, OrderID}).

%% קבלת הזמנות בטווח תאריכים
get_orders_by_date_range(StartDate, EndDate) ->
    gen_server:call(?SERVER, {get_by_date_range, StartDate, EndDate}).

%% קבלת כל ההזמנות של שליח
get_orders_by_courier(CourierID) ->
    gen_server:call(?SERVER, {get_by_courier, CourierID}).

%% קבלת הזמנות לפי מיקום
get_orders_by_location(Location, Radius) ->
    gen_server:call(?SERVER, {get_by_location, Location, Radius}).

%% חיפוש מתקדם בהזמנות
search_orders(Criteria) ->
    gen_server:call(?SERVER, {search, Criteria}).

%% ייצוא היסטוריה לקובץ
export_history(Format) ->
    gen_server:call(?SERVER, {export, Format}).

%% ניקוי היסטוריה ישנה
clear_old_history(DaysToKeep) ->
    gen_server:cast(?SERVER, {clear_old, DaysToKeep}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %% יצירת טבלאות ETS להיסטוריה ואינדקסים
    HistoryTID = ets:new(order_history_table, [ordered_set, named_table, public]),
    DateIndex = ets:new(history_date_index, [ordered_set, public]),
    CourierIndex = ets:new(history_courier_index, [bag, public]),
    LocationIndex = ets:new(history_location_index, [set, public]),
    
    %% טעינת היסטוריה קיימת אם יש (מקובץ או DB)
    load_existing_history(),
    
    %% הפעלת טיימר לניקוי היסטוריה ישנה
    erlang:send_after(86400000, self(), cleanup_old_history), % פעם ביום
    
    io:format("Order history manager started.~n"),
    
    {ok, #state{
        history_tid = HistoryTID,
        index_by_date = DateIndex,
        index_by_courier = CourierIndex,
        index_by_location = LocationIndex,
        total_orders = 0
    }}.

handle_call({get_order, OrderID}, _From, State) ->
    Result = case ets:lookup(State#state.history_tid, OrderID) of
        [{OrderID, Order}] -> {ok, Order};
        [] -> {error, not_found}
    end,
    {reply, Result, State};

handle_call({get_by_date_range, StartDate, EndDate}, _From, State) ->
    %% שימוש באינדקס תאריכים לחיפוש יעיל
    Orders = get_orders_in_date_range(State#state.index_by_date, 
                                      State#state.history_tid, 
                                      StartDate, EndDate),
    {reply, Orders, State};

handle_call({get_by_courier, CourierID}, _From, State) ->
    %% שימוש באינדקס שליחים
    OrderIDs = ets:lookup(State#state.index_by_courier, CourierID),
    Orders = lists:map(fun({_, OrderID}) ->
        [{OrderID, Order}] = ets:lookup(State#state.history_tid, OrderID),
        Order
    end, OrderIDs),
    {reply, Orders, State};

handle_call({get_by_location, Location, Radius}, _From, State) ->
    %% חיפוש הזמנות ברדיוס מסוים ממיקום
    Orders = find_orders_near_location(State#state.history_tid, Location, Radius),
    {reply, Orders, State};

handle_call({search, Criteria}, _From, State) ->
    %% חיפוש מתקדם עם מספר קריטריונים
    Orders = perform_advanced_search(State#state.history_tid, Criteria),
    {reply, Orders, State};

handle_call({export, Format}, _From, State) ->
    %% ייצוא היסטוריה לפורמט מבוקש
    Result = export_history_to_format(State#state.history_tid, Format),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({add_order, OrderData}, State) ->
    OrderID = maps:get(id, OrderData),
    CompletedAt = maps:get(completed_at, OrderData),
    CourierID = maps:get(assigned_courier, OrderData),
    
    %% הוספת נתונים סטטיסטיים להזמנה
    EnrichedOrder = enrich_order_data(OrderData),
    
    %% שמירה בטבלה הראשית
    ets:insert(State#state.history_tid, {OrderID, EnrichedOrder}),
    
    %% עדכון אינדקסים
    ets:insert(State#state.index_by_date, {CompletedAt, OrderID}),
    ets:insert(State#state.index_by_courier, {CourierID, OrderID}),
    update_location_index(State#state.index_by_location, OrderID, EnrichedOrder),
    
    %% שמירה מתמדת (לקובץ או DB)
    persist_order(EnrichedOrder),
    
    io:format("Order ~p added to history.~n", [OrderID]),
    
    {noreply, State#state{total_orders = State#state.total_orders + 1}};

handle_cast({clear_old, DaysToKeep}, State) ->
    %% מחיקת היסטוריה ישנה
    CutoffDate = erlang:system_time(millisecond) - (DaysToKeep * 86400000),
    DeletedCount = clear_orders_before_date(State#state.history_tid, 
                                           State#state.index_by_date, 
                                           CutoffDate),
    io:format("Cleared ~p old orders from history.~n", [DeletedCount]),
    {noreply, State#state{total_orders = State#state.total_orders - DeletedCount}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_old_history, State) ->
    %% ניקוי אוטומטי של היסטוריה ישנה
    gen_server:cast(self(), {clear_old, ?HISTORY_RETENTION_DAYS}),
    
    %% תזמון הניקוי הבא
    erlang:send_after(86400000, self(), cleanup_old_history),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% שמירת כל ההיסטוריה לפני סגירה
    save_all_history(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% פונקציות עזר פנימיות
%% ===================================================================

%% העשרת נתוני הזמנה עם חישובים נוספים
enrich_order_data(OrderData) ->
    CreatedAt = maps:get(created_at, OrderData, 0),
    CompletedAt = maps:get(completed_at, OrderData, 0),
    DeliveryTime = CompletedAt - CreatedAt,
    
    %% חישוב מרחק
    BusinessLoc = maps:get(business_location, OrderData),
    CustomerLoc = maps:get(customer_location, OrderData),
    Distance = calculate_distance(BusinessLoc, CustomerLoc),
    
    OrderData#{
        delivery_time_ms => DeliveryTime,
        delivery_distance => Distance,
        archived_at => erlang:system_time(millisecond)
    }.

%% חישוב מרחק בין שתי נקודות
calculate_distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

%% קבלת הזמנות בטווח תאריכים
get_orders_in_date_range(DateIndex, HistoryTID, StartDate, EndDate) ->
    %% שימוש ב-select לקבלת OrderIDs בטווח
    MatchSpec = [{{'$1', '$2'}, 
                  [{'>=', '$1', StartDate}, {'=<', '$1', EndDate}], 
                  ['$2']}],
    OrderIDs = ets:select(DateIndex, MatchSpec),
    
    %% קבלת ההזמנות המלאות
    lists:map(fun(OrderID) ->
        [{OrderID, Order}] = ets:lookup(HistoryTID, OrderID),
        Order
    end, OrderIDs).

%% חיפוש הזמנות ליד מיקום
find_orders_near_location(HistoryTID, {X, Y}, Radius) ->
    AllOrders = ets:tab2list(HistoryTID),
    lists:filter(fun({_, Order}) ->
        CustomerLoc = maps:get(customer_location, Order),
        Distance = calculate_distance({X, Y}, CustomerLoc),
        Distance =< Radius
    end, AllOrders).

%% חיפוש מתקדם
perform_advanced_search(HistoryTID, Criteria) ->
    AllOrders = ets:tab2list(HistoryTID),
    lists:filter(fun({_, Order}) ->
        matches_criteria(Order, Criteria)
    end, AllOrders).

%% בדיקה אם הזמנה מתאימה לקריטריונים
matches_criteria(Order, Criteria) ->
    lists:all(fun({Key, Value}) ->
        case Key of
            min_delivery_time ->
                maps:get(delivery_time_ms, Order, 0) >= Value;
            max_delivery_time ->
                maps:get(delivery_time_ms, Order, infinity) =< Value;
            courier_id ->
                maps:get(assigned_courier, Order) == Value;
            _ ->
                true
        end
    end, maps:to_list(Criteria)).

%% עדכון אינדקס מיקומים
update_location_index(LocationIndex, OrderID, Order) ->
    CustomerLoc = maps:get(customer_location, Order),
    %% שמירת מיקום בקוורדינטות מעוגלות לאינדקס יעיל
    {X, Y} = CustomerLoc,
    GridX = round(X / 10) * 10,
    GridY = round(Y / 10) * 10,
    ets:insert(LocationIndex, {{GridX, GridY}, OrderID}).

%% ייצוא לפורמט
export_history_to_format(HistoryTID, csv) ->
    %% ייצוא לפורמט CSV
    Orders = ets:tab2list(HistoryTID),
    CSV = create_csv_from_orders(Orders),
    {ok, CSV};
export_history_to_format(HistoryTID, json) ->
    %% ייצוא לפורמט JSON
    Orders = ets:tab2list(HistoryTID),
    {ok, orders_to_json(Orders)};
export_history_to_format(_, Format) ->
    {error, {unsupported_format, Format}}.

%% יצירת CSV מהזמנות
create_csv_from_orders(Orders) ->
    Header = "OrderID,CourierID,DeliveryTime,Distance,CompletedAt\n",
    Rows = lists:map(fun({OrderID, Order}) ->
        io_lib:format("~p,~p,~p,~p,~p~n", [
            OrderID,
            maps:get(assigned_courier, Order, ""),
            maps:get(delivery_time_ms, Order, 0),
            maps:get(delivery_distance, Order, 0),
            maps:get(completed_at, Order, 0)
        ])
    end, Orders),
    Header ++ lists:flatten(Rows).

%% המרת הזמנות ל-JSON
orders_to_json(Orders) ->
    %% יישום בסיסי - במציאות כדאי להשתמש בספריה
    io_lib:format("~p", [Orders]).

%% ניקוי הזמנות ישנות
clear_orders_before_date(HistoryTID, DateIndex, CutoffDate) ->
    %% מציאת הזמנות למחיקה
    MatchSpec = [{{'$1', '$2'}, [{'<', '$1', CutoffDate}], ['$2']}],
    OrdersToDelete = ets:select(DateIndex, MatchSpec),
    
    %% מחיקת הזמנות
    lists:foreach(fun(OrderID) ->
        ets:delete(HistoryTID, OrderID),
        ets:delete(DateIndex, OrderID)
    end, OrdersToDelete),
    
    length(OrdersToDelete).

%% טעינת היסטוריה קיימת
load_existing_history() ->
    %% כאן ניתן לטעון היסטוריה מקובץ או DB
    io:format("Loading existing history (if any)...~n").

%% שמירת הזמנה
persist_order(_Order) ->
    %% כאן ניתן לשמור לקובץ או DB
    ok.

%% שמירת כל ההיסטוריה
save_all_history() ->
    io:format("Saving all history before shutdown...~n").