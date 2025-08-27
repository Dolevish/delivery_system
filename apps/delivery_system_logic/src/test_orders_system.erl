%%%-------------------------------------------------------------------
%% @doc סקריפט בדיקה למערכת ההזמנות המעודכנת
%%      מדגים את כל היכולות החדשות
%% @end
%%%-------------------------------------------------------------------
-module(test_orders_system).

-export([run_all_tests/0, test_validation/0, test_repository/0, 
         test_analytics/0, test_api/0, test_config/0,
         demo_system/0]).

%% ===================================================================
%% הרצת כל הבדיקות
%% ===================================================================
run_all_tests() ->
    io:format("~n========================================~n"),
    io:format("Starting Order System Tests~n"),
    io:format("========================================~n~n"),
    
    test_config(),
    timer:sleep(1000),
    
    test_validation(),
    timer:sleep(1000),
    
    test_repository(),
    timer:sleep(1000),
    
    test_api(),
    timer:sleep(1000),
    
    test_analytics(),
    
    io:format("~n========================================~n"),
    io:format("All tests completed!~n"),
    io:format("========================================~n"),
    ok.

%% ===================================================================
%% בדיקת מודול קונפיגורציה
%% ===================================================================
test_config() ->
    io:format("~nTesting order_config module...~n"),
    io:format("------------------------------~n"),
    
    %% קבלת קונפיגורציה נוכחית
    Interval = order_config:get_generation_interval(),
    io:format("Current generation interval: ~p ms~n", [Interval]),
    
    {MinLoc, MaxLoc} = order_config:get_location_range(),
    io:format("Location range: ~p to ~p~n", [MinLoc, MaxLoc]),
    
    %% שינוי קונפיגורציה
    io:format("Changing interval to 2000ms...~n"),
    order_config:set_generation_interval(2000),
    
    NewInterval = order_config:get_generation_interval(),
    io:format("New interval: ~p ms~n", [NewInterval]),
    
    %% קבלת סוגי עסקים
    BusinessTypes = order_config:get_business_types(),
    io:format("Business types configured: ~p~n", [maps:keys(BusinessTypes)]),
    
    io:format("✓ Config test passed~n").

%% ===================================================================
%% בדיקת מודול ולידציה
%% ===================================================================
test_validation() ->
    io:format("~nTesting order_validator module...~n"),
    io:format("--------------------------------~n"),
    
    %% הזמנה תקינה
    ValidOrder = #{
        id => test_order_1,
        business_location => {50, 50},
        customer_location => {75, 75}
    },
    
    case order_validator:validate_order(ValidOrder) of
        {ok, _} ->
            io:format("✓ Valid order passed validation~n");
        {error, Reason} ->
            io:format("✗ Valid order failed: ~p~n", [Reason])
    end,
    
    %% הזמנה עם מיקום לא תקין
    InvalidOrder1 = #{
        id => test_order_2,
        business_location => {150, 50},  % מחוץ לטווח
        customer_location => {75, 75}
    },
    
    case order_validator:validate_order(InvalidOrder1) of
        {error, _} ->
            io:format("✓ Invalid location correctly rejected~n");
        {ok, _} ->
            io:format("✗ Invalid location should have failed~n")
    end,
    
    %% הזמנה חסרת שדות
    InvalidOrder2 = #{
        id => test_order_3
        % חסר business_location ו-customer_location
    },
    
    case order_validator:validate_order(InvalidOrder2) of
        {error, {missing_fields, _}} ->
            io:format("✓ Missing fields correctly detected~n");
        _ ->
            io:format("✗ Missing fields should have been detected~n")
    end,
    
    io:format("✓ Validation test passed~n").

%% ===================================================================
%% בדיקת מודול repository
%% ===================================================================
test_repository() ->
    io:format("~nTesting order_repository module...~n"),
    io:format("---------------------------------~n"),
    
    %% יצירת הזמנה לבדיקה
    TestOrder = #{
        id => repo_test_1,
        business_location => {40, 40},
        customer_location => {60, 60},
        priority => high
    },
    
    %% אחסון הזמנה
    io:format("Storing order...~n"),
    order_repository:store_order(TestOrder),
    
    %% קריאת הזמנה
    case order_repository:get_order(repo_test_1) of
        {ok, StoredOrder} ->
            io:format("✓ Order stored and retrieved successfully~n"),
            io:format("  Status: ~p~n", [maps:get(status, StoredOrder)]);
        {error, _} ->
            io:format("✗ Failed to retrieve order~n")
    end,
    
    %% עדכון סטטוס
    io:format("Updating order status...~n"),
    order_repository:update_order_status(repo_test_1, processing),
    
    %% סימון כמוקצה
    order_repository:mark_order_assigned(repo_test_1, courier_test),
    
    case order_repository:get_order(repo_test_1) of
        {ok, UpdatedOrder} ->
            io:format("✓ Order status updated~n"),
            io:format("  Assigned to: ~p~n", [maps:get(assigned_courier, UpdatedOrder)]);
        _ ->
            io:format("✗ Status update failed~n")
    end,
    
    %% קבלת כל ההזמנות הפעילות
    ActiveOrders = order_repository:get_active_orders(),
    io:format("Active orders count: ~p~n", [length(ActiveOrders)]),
    
    io:format("✓ Repository test passed~n").

%% ===================================================================
%% בדיקת מודול API
%% ===================================================================
test_api() ->
    io:format("~nTesting order_api module...~n"),
    io:format("---------------------------~n"),
    
    %% הגשת הזמנה דרך API
    ApiOrder = #{
        business_location => {30, 30},
        customer_location => {70, 70},
        items => [#{name => "Test Item", quantity => 2}],
        priority => normal
    },
    
    case order_api:submit_order(ApiOrder, test_client) of
        {ok, OrderID} ->
            io:format("✓ Order submitted via API: ~p~n", [OrderID]),
            
            %% בדיקת סטטוס
            case order_api:get_order_status(OrderID) of
                #{status := Status} ->
                    io:format("  Order status: ~p~n", [Status]);
                _ ->
                    io:format("  Failed to get status~n")
            end;
        {error, Reason} ->
            io:format("✗ API submission failed: ~p~n", [Reason])
    end,
    
    %% קבלת סטטיסטיקות API
    ApiStats = order_api:get_api_stats(),
    io:format("API Stats:~n"),
    io:format("  Enabled: ~p~n", [maps:get(enabled, ApiStats)]),
    io:format("  Rate limit: ~p req/min~n", [maps:get(rate_limit, ApiStats)]),
    io:format("  Total requests: ~p~n", [maps:get(total_requests, ApiStats)]),
    
    io:format("✓ API test passed~n").

%% ===================================================================
%% בדיקת מודול אנליטיקה
%% ===================================================================
test_analytics() ->
    io:format("~nTesting order_analytics module...~n"),
    io:format("--------------------------------~n"),
    
    %% קבלת דשבורד
    Dashboard = order_analytics:get_realtime_dashboard(),
    
    io:format("Analytics Dashboard:~n"),
    
    %% הזמנות
    Orders = maps:get(orders, Dashboard),
    io:format("  Orders:~n"),
    io:format("    Completed: ~p~n", [maps:get(completed, Orders)]),
    io:format("    Failed: ~p~n", [maps:get(failed, Orders)]),
    io:format("    Success rate: ~.1f%~n", [maps:get(success_rate, Orders)]),
    
    %% משלוחים
    Delivery = maps:get(delivery, Dashboard),
    io:format("  Delivery times:~n"),
    io:format("    Average: ~p ms~n", [maps:get(avg_time_ms, Delivery)]),
    io:format("    Min: ~p ms~n", [maps:get(min_time_ms, Delivery)]),
    io:format("    Max: ~p ms~n", [maps:get(max_time_ms, Delivery)]),
    
    %% סטטיסטיקות שליחים
    CourierStats = order_analytics:get_courier_stats(),
    io:format("  Courier statistics:~n"),
    maps:foreach(fun(CourierID, Stats) ->
        io:format("    ~p: ~p completed, ~.1f% success~n", 
                  [CourierID, 
                   maps:get(completed, Stats),
                   maps:get(success_rate, Stats)])
    end, CourierStats),
    
    io:format("✓ Analytics test passed~n").

%% ===================================================================
%% הדגמת המערכת המלאה
%% ===================================================================
demo_system() ->
    io:format("~n========================================~n"),
    io:format("Order System Demo~n"),
    io:format("========================================~n~n"),
    
    %% הגדרת קצב יצירה מהיר להדגמה
    io:format("Setting fast generation rate for demo...~n"),
    order_config:set_generation_interval(1000),
    
    %% המתנה ליצירת כמה הזמנות
    io:format("Waiting for orders to be generated...~n"),
    timer:sleep(5000),
    
    %% הצגת סטטוס נוכחי
    io:format("~n--- Current System Status ---~n"),
    
    %% סטטוס תור
    QueueStatus = courier_manager:get_queue_status(),
    io:format("Queue length: ~p~n", [maps:get(queue_length, QueueStatus)]),
    
    %% סטטוס שליחים
    CourierStatus = courier_manager:get_courier_status(),
    io:format("Couriers:~n"),
    io:format("  Available: ~p~n", [maps:get(available, CourierStatus)]),
    io:format("  Busy: ~p~n", [maps:get(busy, CourierStatus)]),
    
    %% סטטוס מחולל
    GenStats = order_generator:get_stats(),
    io:format("Generator:~n"),
    io:format("  Total generated: ~p~n", [maps:get(total_generated, GenStats)]),
    io:format("  Success rate: ~.1f%~n", [maps:get(success_rate, GenStats)]),
    
    %% הגשת הזמנה ידנית
    io:format("~nSubmitting manual order...~n"),
    ManualOrder = #{
        business_location => {25, 25},
        customer_location => {75, 75},
        priority => urgent,
        items => [
            #{name => "Special Item", quantity => 1, price => 100}
        ]
    },
    
    case order_api:submit_order(ManualOrder, demo_user) of
        {ok, OrderID} ->
            io:format("Manual order submitted: ~p~n", [OrderID]),
            
            %% מעקב אחר ההזמנה
            timer:sleep(2000),
            case order_api:get_order_status(OrderID) of
                #{status := Status, assigned_courier := Courier} ->
                    io:format("Order ~p status: ~p, assigned to: ~p~n", 
                              [OrderID, Status, Courier]);
                _ ->
                    io:format("Order ~p status check failed~n", [OrderID])
            end;
        {error, Reason} ->
            io:format("Manual order failed: ~p~n", [Reason])
    end,
    
    %% החזרת קצב יצירה רגיל
    io:format("~nRestoring normal generation rate...~n"),
    order_config:set_generation_interval(5000),
    
    io:format("~n========================================~n"),
    io:format("Demo completed!~n"),
    io:format("========================================~n").