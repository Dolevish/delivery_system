%%%-------------------------------------------------------------------
%% @doc graphics_server - ממשק גרפי למערכת המשלוחים
%%      משתמש ב-wxWidgets להצגת מפה עם שליחים והזמנות
%%      כרגע עובד במצב דמו עצמאי
%% @end
%%%-------------------------------------------------------------------
-module(graphics_server).
-behaviour(gen_server).

%% API
-export([start_link/0, start/0, stop/0]).
-export([update_courier/3, add_order/3, remove_order/1, demo_mode/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

%% wx includes
-include_lib("wx/include/wx.hrl").

-define(SERVER, ?MODULE).
-define(MAP_WIDTH, 800).
-define(MAP_HEIGHT, 600).
-define(INFO_WIDTH, 300).
-define(REFRESH_INTERVAL, 100). % רענון כל 100ms

%% צבעים
-define(COLOR_BACKGROUND, {240, 240, 240}).
-define(COLOR_MAP_BG, {255, 255, 255}).
-define(COLOR_COURIER_AVAILABLE, {0, 255, 0}).     % ירוק לשליח זמין
-define(COLOR_COURIER_BUSY, {255, 165, 0}).        % כתום לשליח תפוס
-define(COLOR_BUSINESS, {0, 0, 255}).               % כחול לעסק
-define(COLOR_CUSTOMER, {255, 0, 0}).               % אדום ללקוח
-define(COLOR_GRID, {200, 200, 200}).               % אפור לרשת

%% State record
-record(state, {
    wx_server,      % wx server reference
    frame,          % החלון הראשי
    map_panel,      % פאנל המפה
    info_panel,     % פאנל המידע
    control_panel,  % פאנל הבקרה
    canvas,         % Canvas לציור המפה
    timer,          % טיימר לרענון
    couriers = #{}, % מידע על שליחים
    orders = #{},   % מידע על הזמנות
    demo_data = #{} % נתוני דמו
}).

%% ===================================================================
%% API
%% ===================================================================

%% התחלה רגילה דרך supervisor
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% התחלה עצמאית לבדיקות
start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

%% עצירת השרת
stop() ->
    gen_server:stop(?SERVER).

%% עדכון מיקום שליח
update_courier(CourierID, Location, Status) ->
    gen_server:cast(?SERVER, {update_courier, CourierID, Location, Status}).

%% הוספת הזמנה חדשה
add_order(OrderID, BusinessLoc, CustomerLoc) ->
    gen_server:cast(?SERVER, {add_order, OrderID, BusinessLoc, CustomerLoc}).

%% הסרת הזמנה
remove_order(OrderID) ->
    gen_server:cast(?SERVER, {remove_order, OrderID}).

%% הפעלת מצב דמו
demo_mode() ->
    gen_server:cast(?SERVER, start_demo).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %% אתחול שרת wx
    WxServer = wx:new(),
    
    %% יצירת החלון הראשי
    Frame = wxFrame:new(WxServer, ?wxID_ANY, "מערכת ניהול משלוחים - מצב דמו", 
                        [{size, {?MAP_WIDTH + ?INFO_WIDTH, ?MAP_HEIGHT + 100}}]),
    
    %% יצירת פאנל ראשי עם BoxSizer
    MainPanel = wxPanel:new(Frame),
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    
    %% יצירת הפאנלים השונים
    MapPanel = create_map_panel(MainPanel),
    InfoPanel = create_info_panel(MainPanel),
    
    %% הוספת הפאנלים ל-sizer
    wxSizer:add(MainSizer, MapPanel, [{proportion, 2}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, InfoPanel, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    
    wxPanel:setSizer(MainPanel, MainSizer),
    
    %% יצירת Canvas לציור המפה
    Canvas = wxPanel:new(MapPanel, [{size, {?MAP_WIDTH, ?MAP_HEIGHT}}]),
    
    %% חיבור לאירועי ציור
    wxPanel:connect(Canvas, paint, [{callback, fun handle_paint/2}]),
    
    %% חיבור לאירועי עכבר
    wxPanel:connect(Canvas, left_down),
    wxPanel:connect(Canvas, right_down),
    wxPanel:connect(Canvas, motion),
    
    %% יצירת תפריט
    create_menu_bar(Frame),
    
    %% יצירת status bar
    StatusBar = wxFrame:createStatusBar(Frame),
    wxStatusBar:setStatusText(StatusBar, "מערכת מוכנה - מצב דמו"),
    
    %% הצגת החלון
    wxFrame:show(Frame),
    
    %% הפעלת טיימר לרענון התצוגה
    Timer = erlang:send_after(?REFRESH_INTERVAL, self(), refresh),
    
    %% אתחול state עם נתוני דמו
    State = #state{
        wx_server = WxServer,
        frame = Frame,
        map_panel = MapPanel,
        info_panel = InfoPanel,
        canvas = Canvas,
        timer = Timer,
        couriers = init_demo_couriers(),
        orders = init_demo_orders()
    },
    
    io:format("Graphics server started in demo mode~n"),
    
    {ok, State}.

%% טיפול בקריאות סינכרוניות
handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% טיפול בהודעות אסינכרוניות

%% הפעלת מצב דמו עם אנימציה
handle_cast(start_demo, State) ->
    io:format("Starting demo animation~n"),
    %% התחל אנימציה של תנועת שליחים
    erlang:send_after(1000, self(), demo_move),
    {noreply, State};

%% עדכון מיקום שליח
handle_cast({update_courier, CourierID, Location, Status}, State = #state{couriers = Couriers}) ->
    UpdatedCouriers = maps:put(CourierID, #{location => Location, status => Status}, Couriers),
    {noreply, State#state{couriers = UpdatedCouriers}};

%% הוספת הזמנה
handle_cast({add_order, OrderID, BusinessLoc, CustomerLoc}, State = #state{orders = Orders}) ->
    NewOrder = #{
        id => OrderID,
        business_location => BusinessLoc,
        customer_location => CustomerLoc,
        status => waiting
    },
    UpdatedOrders = maps:put(OrderID, NewOrder, Orders),
    {noreply, State#state{orders = UpdatedOrders}};

%% הסרת הזמנה
handle_cast({remove_order, OrderID}, State = #state{orders = Orders}) ->
    UpdatedOrders = maps:remove(OrderID, Orders),
    {noreply, State#state{orders = UpdatedOrders}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% טיפול בהודעות info

%% רענון התצוגה
handle_info(refresh, State = #state{canvas = Canvas, timer = OldTimer}) ->
    %% בטל טיימר ישן אם קיים
    catch erlang:cancel_timer(OldTimer),
    
    %% צייר מחדש את המפה
    wxPanel:refresh(Canvas),
    
    %% הגדר טיימר חדש
    NewTimer = erlang:send_after(?REFRESH_INTERVAL, self(), refresh),
    
    {noreply, State#state{timer = NewTimer}};

%% אנימציה במצב דמו - הזז שליחים
handle_info(demo_move, State = #state{couriers = Couriers}) ->
    %% הזז כל שליח קצת
    UpdatedCouriers = maps:map(fun(_ID, Courier) ->
        {X, Y} = maps:get(location, Courier),
        Status = maps:get(status, Courier),
        
        %% תנועה אקראית קטנה
        DX = (rand:uniform(11) - 6),  % -5 עד 5
        DY = (rand:uniform(11) - 6),  % -5 עד 5
        
        %% וודא שנשארים בגבולות המפה
        NewX = max(10, min(?MAP_WIDTH - 10, X + DX)),
        NewY = max(10, min(?MAP_HEIGHT - 10, Y + DY)),
        
        Courier#{location => {NewX, NewY}}
    end, Couriers),
    
    %% המשך אנימציה
    erlang:send_after(500, self(), demo_move),
    
    {noreply, State#state{couriers = UpdatedCouriers}};

%% טיפול באירועי עכבר - לחיצה שמאלית
handle_info(#wx{event = #wxMouse{type = left_down, x = X, y = Y}}, 
            State = #state{couriers = Couriers, orders = Orders}) ->
    io:format("לחיצת עכבר במיקום (~p, ~p)~n", [X, Y]),
    
    %% בדוק אם לחצו על שליח
    case find_courier_at_position({X, Y}, Couriers) of
        {ok, CourierID, CourierData} ->
            Status = maps:get(status, CourierData),
            {CX, CY} = maps:get(location, CourierData),
            io:format("~n=== מידע על שליח ===~n"),
            io:format("מזהה: ~p~n", [CourierID]),
            io:format("מיקום: (~p, ~p)~n", [CX, CY]),
            io:format("סטטוס: ~p~n", [Status]),
            io:format("==================~n~n");
        not_found ->
            %% בדוק אם לחצו על הזמנה
            case find_order_at_position({X, Y}, Orders) of
                {ok, OrderID, OrderData} ->
                    {BX, BY} = maps:get(business_location, OrderData),
                    {CX, CY} = maps:get(customer_location, OrderData),
                    io:format("~n=== מידע על הזמנה ===~n"),
                    io:format("מזהה: ~p~n", [OrderID]),
                    io:format("איסוף: (~p, ~p)~n", [BX, BY]),
                    io:format("יעד: (~p, ~p)~n", [CX, CY]),
                    io:format("סטטוס: ~p~n", [maps:get(status, OrderData)]),
                    io:format("==================~n~n");
                not_found ->
                    ok
            end
    end,
    {noreply, State};

%% טיפול באירועי עכבר - לחיצה ימנית (הוסף שליח או הזמנה)
handle_info(#wx{event = #wxMouse{type = right_down, x = X, y = Y}}, 
            State = #state{couriers = Couriers, orders = Orders}) ->
    
    %% תפריט הקשר
    io:format("~nלחיצה ימנית במיקום (~p, ~p)~n", [X, Y]),
    io:format("אפשרויות:~n"),
    io:format("1. הוסף שליח חדש במיקום זה~n"),
    io:format("2. הוסף הזמנה חדשה מכאן~n"),
    
    %% לצורך הדמו, נוסיף שליח חדש
    NewCourierID = list_to_atom("courier_" ++ integer_to_list(erlang:unique_integer([positive]))),
    NewCourier = #{location => {X, Y}, status => available},
    UpdatedCouriers = maps:put(NewCourierID, NewCourier, Couriers),
    
    io:format("✓ שליח חדש ~p נוסף במיקום (~p, ~p)~n", [NewCourierID, X, Y]),
    
    {noreply, State#state{couriers = UpdatedCouriers}};

%% טיפול בתנועת עכבר
handle_info(#wx{event = #wxMouse{type = motion, x = X, y = Y}}, State) ->
    %% עדכן את שורת הסטטוס עם המיקום הנוכחי
    wxFrame:setStatusText(State#state.frame, 
                         io_lib:format("מיקום: (~p, ~p)", [X, Y]), 
                         [{number, 0}]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% ניקוי בעת סיום
terminate(_Reason, #state{wx_server = WxServer, timer = Timer}) ->
    catch erlang:cancel_timer(Timer),
    wx:destroy(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions - UI Creation
%% ===================================================================

%% יצירת פאנל המפה
create_map_panel(Parent) ->
    Panel = wxPanel:new(Parent),
    
    %% כותרת
    Title = wxStaticText:new(Panel, ?wxID_ANY, "מפת משלוחים", 
                            [{style, ?wxALIGN_CENTER}]),
    Font = wxFont:new(14, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    wxStaticText:setFont(Title, Font),
    
    %% Sizer
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Title, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    
    wxPanel:setSizer(Panel, Sizer),
    Panel.

%% יצירת פאנל המידע
create_info_panel(Parent) ->
    Panel = wxPanel:new(Parent),
    
    %% כותרת
    Title = wxStaticText:new(Panel, ?wxID_ANY, "מידע מערכת", 
                            [{style, ?wxALIGN_CENTER}]),
    Font = wxFont:new(14, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    wxStaticText:setFont(Title, Font),
    
    %% יצירת תיבות טקסט למידע
    CouriersInfo = wxStaticBox:new(Panel, ?wxID_ANY, "שליחים"),
    CouriersSizer = wxStaticBoxSizer:new(CouriersInfo, ?wxVERTICAL),
    
    CouriersText = wxStaticText:new(Panel, ?wxID_ANY, 
        "שליחים זמינים: 2\nשליחים תפוסים: 1\nסה\"כ שליחים: 3"),
    wxSizer:add(CouriersSizer, CouriersText, [{flag, ?wxALL}, {border, 5}]),
    
    OrdersInfo = wxStaticBox:new(Panel, ?wxID_ANY, "הזמנות"),
    OrdersSizer = wxStaticBoxSizer:new(OrdersInfo, ?wxVERTICAL),
    
    OrdersText = wxStaticText:new(Panel, ?wxID_ANY, 
        "הזמנות ממתינות: 2\nהזמנות בביצוע: 1\nהושלמו היום: 5"),
    wxSizer:add(OrdersSizer, OrdersText, [{flag, ?wxALL}, {border, 5}]),
    
    %% כפתורי פקודות
    StartBtn = wxButton:new(Panel, ?wxID_ANY, [{label, "התחל אנימציה"}]),
    wxButton:connect(StartBtn, command_button_clicked, 
                    [{callback, fun(_Evt, _Obj) -> demo_mode() end}]),
    
    StopBtn = wxButton:new(Panel, ?wxID_ANY, [{label, "עצור"}]),
    wxButton:connect(StopBtn, command_button_clicked,
                    [{callback, fun(_Evt, _Obj) -> io:format("Stop clicked~n") end}]),
    
    %% Sizer ראשי לפאנל
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, Title, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, CouriersSizer, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, OrdersSizer, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, StartBtn, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, StopBtn, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    
    wxPanel:setSizer(Panel, MainSizer),
    Panel.

%% יצירת תפריט
create_menu_bar(Frame) ->
    MenuBar = wxMenuBar:new(),
    
    %% תפריט קובץ
    FileMenu = wxMenu:new(),
    wxMenu:append(FileMenu, ?wxID_NEW, "חדש\tCtrl+N"),
    wxMenu:append(FileMenu, ?wxID_OPEN, "פתח\tCtrl+O"),
    wxMenu:appendSeparator(FileMenu),
    wxMenu:append(FileMenu, ?wxID_EXIT, "יציאה\tAlt+F4"),
    
    %% תפריט תצוגה
    ViewMenu = wxMenu:new(),
    wxMenu:append(ViewMenu, 101, "הגדל\tCtrl++"),
    wxMenu:append(ViewMenu, 102, "הקטן\tCtrl+-"),
    wxMenu:append(ViewMenu, 103, "איפוס תצוגה\tCtrl+0"),
    
    %% תפריט עזרה
    HelpMenu = wxMenu:new(),
    wxMenu:append(HelpMenu, ?wxID_ABOUT, "אודות"),
    
    wxMenuBar:append(MenuBar, FileMenu, "קובץ"),
    wxMenuBar:append(MenuBar, ViewMenu, "תצוגה"),
    wxMenuBar:append(MenuBar, HelpMenu, "עזרה"),
    
    wxFrame:setMenuBar(Frame, MenuBar),
    
    %% חיבור לאירועי תפריט
    wxFrame:connect(Frame, command_menu_selected).

%% ===================================================================
%% Internal functions - Drawing
%% ===================================================================

%% פונקציה לטיפול באירוע ציור
handle_paint(#wx{obj = Canvas}, _State) ->
    %% קבל את ה-state של השרת
    case catch gen_server:call(?SERVER, get_state, 100) of
        {ok, State} ->
            draw_map(Canvas, State);
        _ ->
            %% אם לא הצלחנו לקבל state, צייר משהו בסיסי
            DC = wxPaintDC:new(Canvas),
            draw_basic_map(DC),
            wxPaintDC:destroy(DC)
    end.

%% ציור המפה
draw_map(Canvas, #state{couriers = Couriers, orders = Orders}) ->
    DC = wxPaintDC:new(Canvas),
    
    %% נקה את הרקע
    wxDC:setBackground(DC, wxBrush:new(?COLOR_MAP_BG)),
    wxDC:clear(DC),
    
    %% צייר רשת
    draw_grid(DC),
    
    %% צייר הזמנות
    maps:foreach(fun(_ID, Order) ->
        draw_order(DC, Order)
    end, Orders),
    
    %% צייר שליחים
    maps:foreach(fun(ID, Courier) ->
        draw_courier(DC, ID, Courier)
    end, Couriers),
    
    %% צייר מקרא
    draw_legend(DC),
    
    wxPaintDC:destroy(DC),
    ok.

%% ציור מפה בסיסית (למקרה שאין נתונים)
draw_basic_map(DC) ->
    %% נקה את הרקע
    wxDC:setBackground(DC, wxBrush:new(?COLOR_MAP_BG)),
    wxDC:clear(DC),
    
    %% צייר רשת
    draw_grid(DC),
    
    %% צייר מקרא
    draw_legend(DC),
    ok.

%% ציור רשת על המפה
draw_grid(DC) ->
    wxDC:setPen(DC, wxPen:new(?COLOR_GRID, [{width, 1}, {style, ?wxDOT}])),
    
    %% קווים אנכיים
    lists:foreach(fun(X) ->
        wxDC:drawLine(DC, {X, 0}, {X, ?MAP_HEIGHT})
    end, lists:seq(0, ?MAP_WIDTH, 50)),
    
    %% קווים אופקיים
    lists:foreach(fun(Y) ->
        wxDC:drawLine(DC, {0, Y}, {?MAP_WIDTH, Y})
    end, lists:seq(0, ?MAP_HEIGHT, 50)).

%% ציור שליח
draw_courier(DC, ID, #{location := {X, Y}, status := Status}) ->
    %% בחר צבע לפי סטטוס
    Color = case Status of
        available -> ?COLOR_COURIER_AVAILABLE;
        busy -> ?COLOR_COURIER_BUSY;
        _ -> {128, 128, 128}
    end,
    
    %% צייר עיגול לשליח
    wxDC:setBrush(DC, wxBrush:new(Color)),
    wxDC:setPen(DC, wxPen:new({0, 0, 0}, [{width, 2}])),
    wxDC:drawCircle(DC, {round(X), round(Y)}, 10),
    
    %% צייר מספר שליח
    wxDC:setTextForeground(DC, {0, 0, 0}),
    IDStr = case ID of
        courier_1 -> "1";
        courier_2 -> "2";
        courier_3 -> "3";
        _ -> "?"
    end,
    wxDC:drawText(DC, IDStr, {round(X) - 4, round(Y) - 8}).

%% ציור הזמנה
draw_order(DC, #{business_location := {BX, BY}, customer_location := {CX, CY}}) ->
    %% צייר ריבוע לעסק
    wxDC:setBrush(DC, wxBrush:new(?COLOR_BUSINESS)),
    wxDC:setPen(DC, wxPen:new({0, 0, 0}, [{width, 2}])),
    wxDC:drawRectangle(DC, {round(BX) - 8, round(BY) - 8}, {16, 16}),
    
    %% צייר משולש ללקוח
    wxDC:setBrush(DC, wxBrush:new(?COLOR_CUSTOMER)),
    Points = [{round(CX), round(CY) - 10}, 
              {round(CX) - 8, round(CY) + 8}, 
              {round(CX) + 8, round(CY) + 8}],
    wxDC:drawPolygon(DC, Points),
    
    %% צייר קו מקווקו בין עסק ללקוח
    wxDC:setPen(DC, wxPen:new({128, 128, 128}, [{width, 1}, {style, ?wxDOT}])),
    wxDC:drawLine(DC, {round(BX), round(BY)}, {round(CX), round(CY)}).

%% ציור מקרא
draw_legend(DC) ->
    StartX = ?MAP_WIDTH - 150,
    StartY = 10,
    
    %% רקע למקרא
    wxDC:setBrush(DC, wxBrush:new({255, 255, 240})),
    wxDC:setPen(DC, wxPen:new({0, 0, 0}, [{width, 1}])),
    wxDC:drawRectangle(DC, {StartX - 5, StartY - 5}, {140, 100}),
    
    %% כותרת
    wxDC:setTextForeground(DC, {0, 0, 0}),
    wxDC:drawText(DC, "מקרא:", {StartX, StartY}),
    
    %% שליח זמין
    wxDC:setBrush(DC, wxBrush:new(?COLOR_COURIER_AVAILABLE)),
    wxDC:drawCircle(DC, {StartX + 10, StartY + 25}, 5),
    wxDC:drawText(DC, "שליח זמין", {StartX + 25, StartY + 20}),
    
    %% שליח תפוס
    wxDC:setBrush(DC, wxBrush:new(?COLOR_COURIER_BUSY)),
    wxDC:drawCircle(DC, {StartX + 10, StartY + 45}, 5),
    wxDC:drawText(DC, "שליח תפוס", {StartX + 25, StartY + 40}),
    
    %% עסק
    wxDC:setBrush(DC, wxBrush:new(?COLOR_BUSINESS)),
    wxDC:drawRectangle(DC, {StartX + 5, StartY + 60}, {10, 10}),
    wxDC:drawText(DC, "איסוף", {StartX + 25, StartY + 60}),
    
    %% לקוח
    wxDC:setBrush(DC, wxBrush:new(?COLOR_CUSTOMER)),
    Points = [{StartX + 10, StartY + 75}, 
              {StartX + 5, StartY + 85}, 
              {StartX + 15, StartY + 85}],
    wxDC:drawPolygon(DC, Points),
    wxDC:drawText(DC, "יעד", {StartX + 25, StartY + 80}).

%% ===================================================================
%% Internal functions - UI Interaction
%% ===================================================================

%% מצא שליח במיקום מסוים
find_courier_at_position({X, Y}, Couriers) ->
    Threshold = 15, % רדיוס לזיהוי לחיצה
    
    Found = maps:fold(fun(ID, CourierData, Acc) ->
        {CX, CY} = maps:get(location, CourierData),
        Distance = math:sqrt(math:pow(X - CX, 2) + math:pow(Y - CY, 2)),
        if
            Distance =< Threshold -> {ok, ID, CourierData};
            true -> Acc
        end
    end, not_found, Couriers),
    
    Found.

%% מצא הזמנה במיקום מסוים
find_order_at_position({X, Y}, Orders) ->
    Threshold = 15, % רדיוס לזיהוי לחיצה
    
    Found = maps:fold(fun(ID, OrderData, Acc) ->
        {BX, BY} = maps:get(business_location, OrderData),
        {CX, CY} = maps:get(customer_location, OrderData),
        
        %% בדוק מרחק מהעסק
        BusinessDist = math:sqrt(math:pow(X - BX, 2) + math:pow(Y - BY, 2)),
        %% בדוק מרחק מהלקוח
        CustomerDist = math:sqrt(math:pow(X - CX, 2) + math:pow(Y - CY, 2)),
        
        if
            BusinessDist =< Threshold -> {ok, ID, OrderData};
            CustomerDist =< Threshold -> {ok, ID, OrderData};
            true -> Acc
        end
    end, not_found, Orders),
    
    Found.

%% ===================================================================
%% Demo Data Initialization
%% ===================================================================

%% אתחול שליחים לדמו
init_demo_couriers() ->
    #{
        courier_1 => #{location => {100, 150}, status => available},
        courier_2 => #{location => {400, 300}, status => busy},
        courier_3 => #{location => {250, 450}, status => available}
    }.

%% אתחול הזמנות לדמו
init_demo_orders() ->
    #{
        order_1 => #{
            id => order_1,
            business_location => {200, 200},
            customer_location => {500, 400},
            status => in_progress
        },
        order_2 => #{
            id => order_2,
            business_location => {600, 100},
            customer_location => {700, 500},
            status => waiting
        }
    }.