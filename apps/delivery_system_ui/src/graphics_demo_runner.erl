%%%-------------------------------------------------------------------
%% @doc graphics_demo_runner - מודול עזר להרצת הדמו הגרפי
%%      מספק פונקציות נוחות לבדיקה והדגמה
%% @end
%%%-------------------------------------------------------------------
-module(graphics_demo_runner).

-include("city_generator.hrl").
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

    %% קוד חדש - קבל את נתוני העיר מהשרת כדי למקם אובייקטים
    case city_generator:get_city_data() of
        #city{roads=Roads, businesses=Businesses, houses=Houses} when Roads /= [], Businesses /= [], Houses /= [] ->
            %% עדכן מיקומי שליחים
            graphics_server:update_courier(courier_1, element(1, city_generator:find_nearest_road_point({100, 100}, Roads)), available),
            graphics_server:update_courier(courier_2, element(1, city_generator:find_nearest_road_point({400, 400}, Roads)), available),
            graphics_server:update_courier(courier_3, element(1, city_generator:find_nearest_road_point({700, 200}, Roads)), available),
            io:format("✓ 3 שליחים במצב זמין~n"),

            %% הוסף הזמנות
            B1 = lists:nth(rand:uniform(length(Businesses)), Businesses),
            H1 = lists:nth(rand:uniform(length(Houses)), Houses),
            timer:sleep(500),
            graphics_server:add_order(order_101, B1#business.location, H1#house.location),
            io:format("✓ הזמנה 101 נוספה~n"),

            B2 = lists:nth(rand:uniform(length(Businesses)), Businesses),
            H2 = lists:nth(rand:uniform(length(Houses)), Houses),
            timer:sleep(500),
            graphics_server:add_order(order_102, B2#business.location, H2#house.location),
            io:format("✓ הזמנה 102 נוספה~n"),

            %% שנה סטטוס שליחים
            timer:sleep(1000),
            graphics_server:update_courier(courier_1, B1#business.location, busy),
            io:format("✓ שליח 1 התחיל משלוח~n"),

            timer:sleep(1000),
            graphics_server:update_courier(courier_2, B2#business.location, busy),
            io:format("✓ שליח 2 התחיל משלוח~n"),

            %% סימולציה של השלמת משלוח
            timer:sleep(2000),
            graphics_server:update_courier(courier_1, H1#house.location, available),
            graphics_server:remove_order(order_101),
            io:format("✓ שליח 1 השלים משלוח 101~n");
        _ ->
            io:format("! שגיאה: לא נמצאו נתוני עיר תקינים להרצת התרחיש.~n")
    end,

    io:format("~n--- תרחיש 1 הושלם ---~n"),
    ok.

%% תרחיש בדיקה 2 - עומס גבוה
test_scenario_2() ->
    io:format("~n--- מפעיל תרחיש בדיקה 2: עומס גבוה ---~n"),

    %% קוד חדש - קבל נתוני עיר
    case city_generator:get_city_data() of
        #city{businesses=Businesses, houses=Houses} when Businesses /= [], Houses /= [] ->
            %% הוסף הרבה הזמנות בבת אחת
            lists:foreach(fun(N) ->
                OrderID = list_to_atom("order_" ++ integer_to_list(N)),
                Business = lists:nth(rand:uniform(length(Businesses)), Businesses),
                House = lists:nth(rand:uniform(length(Houses)), Houses),
                graphics_server:add_order(OrderID, Business#business.location, House#house.location),
                io:format("✓ הזמנה ~p נוספה~n", [OrderID]),
                timer:sleep(200)
            end, lists:seq(201, 210));
        _ ->
             io:format("! שגיאה: לא נמצאו נתוני עיר תקינים להרצת התרחיש.~n")
    end,

    %% כל השליחים עסוקים
    graphics_server:update_courier(courier_1, {400, 300}, busy),
    graphics_server:update_courier(courier_2, {200, 400}, busy),
    graphics_server:update_courier(courier_3, {600, 200}, busy),

    io:format("~n! כל השליחים תפוסים - 10 הזמנות בתור!~n"),
    io:format("~n--- תרחיש 2 הושלם ---~n"),
    ok.

%% הוספת הזמנה אקראית
add_random_order() ->
    %% קוד חדש - קבל נתוני עיר
    case city_generator:get_city_data() of
        #city{businesses=Businesses, houses=Houses} when Businesses /= [], Houses /= [] ->
            OrderID = list_to_atom("order_" ++ integer_to_list(rand:uniform(9999))),
            Business = lists:nth(rand:uniform(length(Businesses)), Businesses),
            House = lists:nth(rand:uniform(length(Houses)), Houses),
            {BX, BY} = Business#business.location,
            {CX, CY} = House#house.location,

            graphics_server:add_order(OrderID, {BX, BY}, {CX, CY}),
            io:format("✓ הזמנה אקראית ~p נוספה: עסק(~p,~p) -> לקוח(~p,~p)~n",
                      [OrderID, BX, BY, CX, CY]);
        _ ->
            io:format("! שגיאה: לא נמצאו נתוני עיר תקינים להוספת הזמנה.~n")
    end,
    ok.


%% הזז את כל השליחים למיקום אקראי
move_all_couriers() ->
    io:format("~nמזיז שליחים למיקומים אקראיים...~n"),

    %% קוד חדש - קבל נתוני עיר כדי להזיז לכבישים
    case city_generator:get_city_data() of
        #city{roads=Roads} when Roads /= [] ->
            lists:foreach(fun(CourierID) ->
                {X, Y} = element(1, city_generator:find_nearest_road_point(
                                      {rand:uniform(800), rand:uniform(600)}, Roads)),
                Status = case rand:uniform(2) of
                    1 -> available;
                    2 -> busy
                end,
                graphics_server:update_courier(CourierID, {X, Y}, Status),
                io:format("✓ ~p -> (~p,~p) [~p]~n", [CourierID, X, Y, Status])
            end, [courier_1, courier_2, courier_3]);
        _ ->
            io:format("! שגיאה: לא נמצאו נתוני עיר תקינים להזזת שליחים.~n")
    end,
    ok.