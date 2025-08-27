%%%-------------------------------------------------------------------
%% @doc graphics_demo_runner - מודול עזר להרצת הדמו הגרפי
%%      מספק פונקציות נוחות לבדיקה והדגמה
%% @end
%%%-------------------------------------------------------------------
-module(graphics_demo_runner).

%% API
-export([run/0, stop/0, test_scenario_1/0, test_scenario_2/0, 
         add_random_order/0, move_all_couriers/0]).

%% ===================================================================
%% API Functions
%% ===================================================================

%% הרצת הדמו הגרפי
run() ->
    io:format("~n====================================~n"),
    io:format("  מפעיל מערכת ניהול משלוחים גרפית~n"),
    io:format("====================================~n~n"),
    
    %% הפעל את השרת הגרפי
    case graphics_server:start() of
        {ok, Pid} ->
            io:format("✓ השרת הגרפי הופעל בהצלחה (PID: ~p)~n", [Pid]),
            io:format("~n--- פקודות זמינות ---~n"),
            io:format("graphics_demo_runner:test_scenario_1(). - תרחיש בדיקה 1~n"),
            io:format("graphics_demo_runner:test_scenario_2(). - תרחיש בדיקה 2~n"),
            io:format("graphics_demo_runner:add_random_order(). - הוסף הזמנה אקראית~n"),
            io:format("graphics_demo_runner:move_all_couriers(). - הזז את כל השליחים~n"),
            io:format("graphics_server:demo_mode(). - הפעל אנימציה אוטומטית~n"),
            io:format("graphics_demo_runner:stop(). - עצור את המערכת~n"),
            io:format("~n"),
            
            %% הפעל אנימציה אוטומטית אחרי שנייה
            timer:apply_after(1000, graphics_server, demo_mode, []),
            
            {ok, Pid};
        {error, {already_started, Pid}} ->
            io:format("! השרת הגרפי כבר פועל (PID: ~p)~n", [Pid]),
            {ok, Pid};
        Error ->
            io:format("✗ שגיאה בהפעלת השרת: ~p~n", [Error]),
            Error
    end.

%% עצירת הדמו
stop() ->
    io:format("~nעוצר את המערכת הגרפית...~n"),
    graphics_server:stop(),
    io:format("✓ המערכת נעצרה~n"),
    ok.

%% תרחיש בדיקה 1 - סימולציה של יום עבודה רגיל
test_scenario_1() ->
    io:format("~n--- מפעיל תרחיש בדיקה 1: יום עבודה רגיל ---~n"),
    
    %% עדכן מיקומי שליחים
    graphics_server:update_courier(courier_1, {100, 100}, available),
    graphics_server:update_courier(courier_2, {400, 400}, available),
    graphics_server:update_courier(courier_3, {700, 200}, available),
    
    io:format("✓ 3 שליחים במצב זמין~n"),
    
    %% הוסף הזמנות
    timer:sleep(500),
    graphics_server:add_order(order_101, {150, 250}, {600, 450}),
    io:format("✓ הזמנה 101 נוספה~n"),
    
    timer:sleep(500),
    graphics_server:add_order(order_102, {50, 50}, {750, 550}),
    io:format("✓ הזמנה 102 נוספה~n"),
    
    timer:sleep(500),
    graphics_server:add_order(order_103, {400, 100}, {400, 500}),
    io:format("✓ הזמנה 103 נוספה~n"),
    
    %% שנה סטטוס שליחים
    timer:sleep(1000),
    graphics_server:update_courier(courier_1, {150, 250}, busy),
    io:format("✓ שליח 1 התחיל משלוח~n"),
    
    timer:sleep(1000),
    graphics_server:update_courier(courier_2, {50, 50}, busy),
    io:format("✓ שליח 2 התחיל משלוח~n"),
    
    %% סימולציה של השלמת משלוח
    timer:sleep(2000),
    graphics_server:update_courier(courier_1, {600, 450}, available),
    graphics_server:remove_order(order_101),
    io:format("✓ שליח 1 השלים משלוח 101~n"),
    
    io:format("~n--- תרחיש 1 הושלם ---~n"),
    ok.

%% תרחיש בדיקה 2 - עומס גבוה
test_scenario_2() ->
    io:format("~n--- מפעיל תרחיש בדיקה 2: עומס גבוה ---~n"),
    
    %% הוסף הרבה הזמנות בבת אחת
    lists:foreach(fun(N) ->
        OrderID = list_to_atom("order_" ++ integer_to_list(N)),
        BX = rand:uniform(800),
        BY = rand:uniform(600),
        CX = rand:uniform(800),
        CY = rand:uniform(600),
        graphics_server:add_order(OrderID, {BX, BY}, {CX, CY}),
        io:format("✓ הזמנה ~p נוספה~n", [OrderID]),
        timer:sleep(200)
    end, lists:seq(201, 210)),
    
    %% כל השליחים עסוקים
    graphics_server:update_courier(courier_1, {400, 300}, busy),
    graphics_server:update_courier(courier_2, {200, 400}, busy),
    graphics_server:update_courier(courier_3, {600, 200}, busy),
    
    io:format("~n! כל השליחים תפוסים - 10 הזמנות בתור!~n"),
    io:format("~n--- תרחיש 2 הושלם ---~n"),
    ok.

%% הוספת הזמנה אקראית
add_random_order() ->
    OrderID = list_to_atom("order_" ++ integer_to_list(rand:uniform(9999))),
    BX = rand:uniform(800),
    BY = rand:uniform(600),
    CX = rand:uniform(800),
    CY = rand:uniform(600),
    
    graphics_server:add_order(OrderID, {BX, BY}, {CX, CY}),
    io:format("✓ הזמנה אקראית ~p נוספה: עסק(~p,~p) -> לקוח(~p,~p)~n", 
              [OrderID, BX, BY, CX, CY]),
    ok.

%% הזז את כל השליחים למיקום אקראי
move_all_couriers() ->
    io:format("~nמזיז שליחים למיקומים אקראיים...~n"),
    
    lists:foreach(fun(CourierID) ->
        X = rand:uniform(800),
        Y = rand:uniform(600),
        Status = case rand:uniform(2) of
            1 -> available;
            2 -> busy
        end,
        graphics_server:update_courier(CourierID, {X, Y}, Status),
        io:format("✓ ~p -> (~p,~p) [~p]~n", [CourierID, X, Y, Status])
    end, [courier_1, courier_2, courier_3]),
    
    ok.