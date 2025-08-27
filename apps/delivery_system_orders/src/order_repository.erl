%%%-------------------------------------------------------------------
%% @doc order_repository - gen_server לניהול מרכזי של נתוני הזמנות
%%      מנהל ETS tables עבור הזמנות פעילות, שהושלמו ושנכשלו
%%      שומר היסטוריה מלאה של כל הזמנה כולל טיימסטמפים וסטטוסים
%% @end
%%%-------------------------------------------------------------------
-module(order_repository).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([store_order/1, get_order/1, update_order_status/2, 
         mark_order_assigned/2, mark_order_completed/1, mark_order_failed/2,
         get_active_orders/0, get_completed_orders/0, get_failed_orders/0,
         get_order_history/1, get_orders_by_courier/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    active_tid,      %% ETS table עבור הזמנות פעילות
    completed_tid,   %% ETS table עבור הזמנות שהושלמו
    failed_tid,      %% ETS table עבור הזמנות שנכשלו
    history_tid      %% ETS table עבור היסטוריה מלאה
}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% אחסון הזמנה חדשה במאגר
store_order(OrderData) ->
    gen_server:call(?SERVER, {store_order, OrderData}).

%% קבלת נתוני הזמנה לפי ID
get_order(OrderID) ->
    gen_server:call(?SERVER, {get_order, OrderID}).

%% עדכון סטטוס הזמנה
update_order_status(OrderID, NewStatus) ->
    gen_server:cast(?SERVER, {update_status, OrderID, NewStatus}).

%% סימון הזמנה כמוקצית לשליח
mark_order_assigned(OrderID, CourierID) ->
    gen_server:cast(?SERVER, {mark_assigned, OrderID, CourierID}).

%% סימון הזמנה כהושלמה
mark_order_completed(OrderID) ->
    gen_server:cast(?SERVER, {mark_completed, OrderID}).

%% סימון הזמנה כנכשלה
mark_order_failed(OrderID, Reason) ->
    gen_server:cast(?SERVER, {mark_failed, OrderID, Reason}).

%% קבלת כל ההזמנות הפעילות
get_active_orders() ->
    gen_server:call(?SERVER, get_active_orders).

%% קבלת כל ההזמנות שהושלמו
get_completed_orders() ->
    gen_server:call(?SERVER, get_completed_orders).

%% קבלת כל ההזמנות שנכשלו
get_failed_orders() ->
    gen_server:call(?SERVER, get_failed_orders).

%% קבלת היסטוריה מלאה של הזמנה
get_order_history(OrderID) ->
    gen_server:call(?SERVER, {get_history, OrderID}).

%% קבלת כל ההזמנות של שליח מסוים
get_orders_by_courier(CourierID) ->
    gen_server:call(?SERVER, {get_by_courier, CourierID}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %% יצירת ETS tables לניהול הזמנות
    ActiveTID = ets:new(active_orders, [set, named_table, public]),
    CompletedTID = ets:new(completed_orders, [set, named_table, public]),
    FailedTID = ets:new(failed_orders, [set, named_table, public]),
    HistoryTID = ets:new(order_history, [set, named_table, public]),
    
    io:format("Order repository started with ETS tables initialized.~n"),
    
    {ok, #state{
        active_tid = ActiveTID,
        completed_tid = CompletedTID,
        failed_tid = FailedTID,
        history_tid = HistoryTID
    }}.

%% טיפול בקריאות סינכרוניות
handle_call({store_order, OrderData}, _From, State) ->
    OrderID = maps:get(id, OrderData),
    Timestamp = erlang:system_time(millisecond),
    
    %% הוספת טיימסטמפ ומידע נוסף להזמנה
    EnrichedOrder = OrderData#{
        created_at => Timestamp,
        status => pending,
        status_history => [{pending, Timestamp}],
        assigned_courier => undefined
    },
    
    %% אחסון בטבלת הזמנות פעילות
    ets:insert(State#state.active_tid, {OrderID, EnrichedOrder}),
    
    %% אחסון בהיסטוריה
    ets:insert(State#state.history_tid, {OrderID, EnrichedOrder}),
    
    io:format("Order ~p stored in repository.~n", [OrderID]),
    {reply, ok, State};

handle_call({get_order, OrderID}, _From, State) ->
    %% חיפוש הזמנה בכל הטבלאות
    Result = case ets:lookup(State#state.active_tid, OrderID) of
        [{OrderID, Order}] -> {ok, Order};
        [] ->
            case ets:lookup(State#state.completed_tid, OrderID) of
                [{OrderID, Order}] -> {ok, Order};
                [] ->
                    case ets:lookup(State#state.failed_tid, OrderID) of
                        [{OrderID, Order}] -> {ok, Order};
                        [] -> {error, not_found}
                    end
            end
    end,
    {reply, Result, State};

handle_call(get_active_orders, _From, State) ->
    Orders = ets:tab2list(State#state.active_tid),
    {reply, Orders, State};

handle_call(get_completed_orders, _From, State) ->
    Orders = ets:tab2list(State#state.completed_tid),
    {reply, Orders, State};

handle_call(get_failed_orders, _From, State) ->
    Orders = ets:tab2list(State#state.failed_tid),
    {reply, Orders, State};

handle_call({get_history, OrderID}, _From, State) ->
    Result = case ets:lookup(State#state.history_tid, OrderID) of
        [{OrderID, Order}] -> {ok, Order};
        [] -> {error, not_found}
    end,
    {reply, Result, State};

handle_call({get_by_courier, CourierID}, _From, State) ->
    %% חיפוש הזמנות לפי שליח בכל הטבלאות
    ActiveOrders = filter_by_courier(State#state.active_tid, CourierID),
    CompletedOrders = filter_by_courier(State#state.completed_tid, CourierID),
    
    {reply, #{
        active => ActiveOrders,
        completed => CompletedOrders
    }, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% טיפול בקריאות אסינכרוניות
handle_cast({update_status, OrderID, NewStatus}, State) ->
    Timestamp = erlang:system_time(millisecond),
    update_order_status_internal(OrderID, NewStatus, Timestamp, State),
    {noreply, State};

handle_cast({mark_assigned, OrderID, CourierID}, State) ->
    Timestamp = erlang:system_time(millisecond),
    
    %% עדכון הזמנה עם שליח מוקצה
    case ets:lookup(State#state.active_tid, OrderID) of
        [{OrderID, Order}] ->
            UpdatedOrder = Order#{
                assigned_courier => CourierID,
                assigned_at => Timestamp,
                status => assigned,
                status_history => [{assigned, Timestamp} | maps:get(status_history, Order, [])]
            },
            ets:insert(State#state.active_tid, {OrderID, UpdatedOrder}),
            ets:insert(State#state.history_tid, {OrderID, UpdatedOrder}),
            io:format("Order ~p assigned to courier ~p.~n", [OrderID, CourierID]);
        [] ->
            io:format("Warning: Order ~p not found when marking assigned.~n", [OrderID])
    end,
    {noreply, State};

handle_cast({mark_completed, OrderID}, State) ->
    Timestamp = erlang:system_time(millisecond),
    
    %% העברת הזמנה מפעילות להושלמה
    case ets:lookup(State#state.active_tid, OrderID) of
        [{OrderID, Order}] ->
            CompletedOrder = Order#{
                completed_at => Timestamp,
                status => completed,
                status_history => [{completed, Timestamp} | maps:get(status_history, Order, [])]
            },
            
            %% מחיקה מטבלת פעילות
            ets:delete(State#state.active_tid, OrderID),
            
            %% הוספה לטבלת הושלמו
            ets:insert(State#state.completed_tid, {OrderID, CompletedOrder}),
            
            %% עדכון בהיסטוריה
            ets:insert(State#state.history_tid, {OrderID, CompletedOrder}),
            
            %% שליחת עדכון ל-order_history
            order_history:add_completed_order(CompletedOrder),
            
            %% עדכון סטטיסטיקות
            order_analytics:update_order_completed(CompletedOrder),
            
            io:format("Order ~p marked as completed.~n", [OrderID]);
        [] ->
            io:format("Warning: Order ~p not found when marking completed.~n", [OrderID])
    end,
    {noreply, State};

handle_cast({mark_failed, OrderID, Reason}, State) ->
    Timestamp = erlang:system_time(millisecond),
    
    %% העברת הזמנה מפעילות לנכשלה
    case ets:lookup(State#state.active_tid, OrderID) of
        [{OrderID, Order}] ->
            FailedOrder = Order#{
                failed_at => Timestamp,
                failure_reason => Reason,
                status => failed,
                status_history => [{failed, Timestamp} | maps:get(status_history, Order, [])]
            },
            
            %% מחיקה מטבלת פעילות
            ets:delete(State#state.active_tid, OrderID),
            
            %% הוספה לטבלת נכשלו
            ets:insert(State#state.failed_tid, {OrderID, FailedOrder}),
            
            %% עדכון בהיסטוריה
            ets:insert(State#state.history_tid, {OrderID, FailedOrder}),
            
            io:format("Order ~p marked as failed: ~p.~n", [OrderID, Reason]);
        [] ->
            io:format("Warning: Order ~p not found when marking failed.~n", [OrderID])
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% פונקציות עזר פנימיות
%% ===================================================================

%% סינון הזמנות לפי שליח
filter_by_courier(TableID, CourierID) ->
    MatchSpec = [{{'$1', '$2'}, 
                  [{'==', {map_get, assigned_courier, '$2'}, CourierID}], 
                  [{{'$1', '$2'}}]}],
    ets:select(TableID, MatchSpec).

%% עדכון סטטוס פנימי
update_order_status_internal(OrderID, NewStatus, Timestamp, State) ->
    %% מציאת הזמנה בטבלה הפעילה
    case ets:lookup(State#state.active_tid, OrderID) of
        [{OrderID, Order}] ->
            UpdatedOrder = Order#{
                status => NewStatus,
                status_history => [{NewStatus, Timestamp} | maps:get(status_history, Order, [])]
            },
            ets:insert(State#state.active_tid, {OrderID, UpdatedOrder}),
            ets:insert(State#state.history_tid, {OrderID, UpdatedOrder}),
            io:format("Order ~p status updated to ~p.~n", [OrderID, NewStatus]);
        [] ->
            io:format("Warning: Order ~p not found for status update.~n", [OrderID])
    end.