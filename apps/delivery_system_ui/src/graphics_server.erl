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
-include("city_generator.hrl").

-define(SERVER, ?MODULE).
-define(MAP_WIDTH, 800).
-define(MAP_HEIGHT, 600).
-define(INFO_WIDTH, 300).
-define(REFRESH_INTERVAL, 100).
-define(COURIER_SPEED, 5). %% תיקון: הגדרת מהירות קבועה לשליחים

%% צבעים
-define(COLOR_MAP_BG, {210, 225, 210}).
-define(COLOR_COURIER_AVAILABLE, {0, 255, 0}).
-define(COLOR_COURIER_BUSY, {255, 165, 0}).
-define(COLOR_BUSINESS, {0, 0, 255}).
-define(COLOR_CUSTOMER, {255, 0, 0}).
-define(COLOR_ROAD, {80, 80, 80}).
-define(COLOR_HOUSE, {150, 75, 0}).

-record(state, {
    wx_server,
    frame,
    map_panel,
    info_panel,
    canvas,
    timer,
    city_data,
    couriers = #{},
    orders = #{},
    demo_running = false
}).

%% ===================================================================
%% API
%% ===================================================================

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).
stop() -> gen_server:stop(?SERVER).
update_courier(CourierID, Location, Status) -> gen_server:cast(?SERVER, {update_courier, CourierID, Location, Status}).
add_order(OrderID, BusinessLoc, CustomerLoc) -> gen_server:cast(?SERVER, {add_order, OrderID, BusinessLoc, CustomerLoc}).
remove_order(OrderID) -> gen_server:cast(?SERVER, {remove_order, OrderID}).
demo_mode() -> gen_server:cast(?SERVER, start_demo).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    WxServer = wx:new(),
    Frame = wxFrame:new(WxServer, ?wxID_ANY, "מערכת ניהול משלוחים - מצב דמו",
                        [{size, {?MAP_WIDTH + ?INFO_WIDTH, ?MAP_HEIGHT + 100}}]),
    MainPanel = wxPanel:new(Frame),
    MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
    MapPanel = create_map_panel(MainPanel),
    InfoPanel = create_info_panel(MainPanel),
    wxSizer:add(MainSizer, MapPanel, [{proportion, 2}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, InfoPanel, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxPanel:setSizer(MainPanel, MainSizer),
    Canvas = wxPanel:new(MapPanel, [{size, {?MAP_WIDTH, ?MAP_HEIGHT}}]),
    wxPanel:connect(Canvas, paint, [{callback, fun handle_paint/2}]),
    wxPanel:connect(Canvas, left_down),
    wxPanel:connect(Canvas, right_down),
    wxPanel:connect(Canvas, motion),
    create_menu_bar(Frame),
    StatusBar = wxFrame:createStatusBar(Frame),
    wxStatusBar:setStatusText(StatusBar, "מערכת מוכנה - מצב דמו"),
    wxFrame:show(Frame),
    Timer = erlang:send_after(?REFRESH_INTERVAL, self(), refresh),
    CityData = city_generator:generate_city(#{width => ?MAP_WIDTH, height => ?MAP_HEIGHT}),
    InitialOrders = init_demo_orders(CityData),
    %% תיקון: אתחול השליחים צריך את ההזמנות כדי לדעת לאן לנסוע
    InitialCouriers = init_demo_couriers(CityData, InitialOrders),
    State = #state{
        wx_server = WxServer,
        frame = Frame,
        map_panel = MapPanel,
        info_panel = InfoPanel,
        canvas = Canvas,
        timer = Timer,
        city_data = CityData,
        couriers = InitialCouriers,
        orders = InitialOrders
    },
    io:format("Graphics server started in demo mode~n"),
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(start_demo, State) ->
    io:format("Starting demo animation~n"),
    self() ! demo_move,
    {noreply, State#state{demo_running = true}};
handle_cast({set_demo_running, Flag}, State) when is_boolean(Flag) ->
    io:format("Animation stopped~n"),
    {noreply, State#state{demo_running = Flag}};
handle_cast({remove_order, OrderID}, State = #state{orders = Orders}) ->
    UpdatedOrders = maps:remove(OrderID, Orders),
    {noreply, State#state{orders = UpdatedOrders}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh, State = #state{canvas = Canvas, timer = OldTimer}) ->
    catch erlang:cancel_timer(OldTimer),
    wxPanel:refresh(Canvas),
    NewTimer = erlang:send_after(?REFRESH_INTERVAL, self(), refresh),
    {noreply, State#state{timer = NewTimer}};

handle_info(demo_move, State = #state{demo_running = false}) ->
    {noreply, State};

%% תיקון: כל לוגיקת האנימציה שוכתבה לתנועה מונחית מטרה
handle_info(demo_move, State = #state{couriers = Couriers, orders = Orders, demo_running = true}) ->
    UpdatedCouriers = maps:map(fun(CourierID, CourierData) ->
        case maps:get(status, CourierData, available) of
            busy ->
                Target = maps:get(target_location, CourierData),
                Current = maps:get(location, CourierData),
                if
                    Current == Target ->
                        %% השליח הגיע ליעד, בדוק מה היעד הבא
                        handle_arrival(CourierID, CourierData, Orders);
                    true ->
                        %% השליח בדרך, הזז אותו צעד אחד
                        NewLoc = move_towards(Current, Target),
                        CourierData#{location => NewLoc}
                end;
            _ ->
                %% שליח פנוי לא זז
                CourierData
        end
    end, Couriers),
    NewOrders = maps:filter(fun(_OrderID, Order) ->
        maps:get(status, Order) =/= delivered
    end, Orders),
    erlang:send_after(50, self(), demo_move),
    {noreply, State#state{couriers = UpdatedCouriers, orders = NewOrders}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{timer = Timer}) ->
    catch erlang:cancel_timer(Timer),
    wx:destroy(),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal functions - UI Creation & Drawing (רוב הפונקציות נשארו זהות)
%% ===================================================================
create_map_panel(Parent) -> wxPanel:new(Parent).
create_info_panel(Parent) ->
    Panel = wxPanel:new(Parent),
    Title = wxStaticText:new(Panel, ?wxID_ANY, "פקדים", [{style, ?wxALIGN_CENTER}]),
    Font = wxFont:new(14, ?wxFONTFAMILY_DEFAULT, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_BOLD),
    wxStaticText:setFont(Title, Font),
    StartBtn = wxButton:new(Panel, ?wxID_ANY, [{label, "התחל אנימציה"}]),
    wxButton:connect(StartBtn, command_button_clicked, [{callback, fun(_,_) -> demo_mode() end}]),
    StopBtn = wxButton:new(Panel, ?wxID_ANY, [{label, "עצור אנימציה"}]),
    wxButton:connect(StopBtn, command_button_clicked,
                    [{callback, fun(_,_) -> gen_server:cast(?SERVER, {set_demo_running, false}) end}]),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, Title, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, StartBtn, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(MainSizer, StopBtn, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxPanel:setSizer(Panel, MainSizer),
    Panel.
create_menu_bar(_Frame) -> ok.
handle_paint(#wx{obj = Canvas}, _State) ->
    case catch gen_server:call(?SERVER, get_state, 100) of
        {ok, State} -> draw_map(Canvas, State);
        _ -> DC = wxPaintDC:new(Canvas), wxPaintDC:destroy(DC)
    end.
draw_map(Canvas, #state{couriers = Couriers, orders = Orders, city_data = City}) ->
    DC = wxPaintDC:new(Canvas),
    wxDC:setBackground(DC, wxBrush:new(?COLOR_MAP_BG)),
    wxDC:clear(DC),
    draw_city(DC, City),
    maps:foreach(fun(_ID, Order) -> draw_order(DC, Order) end, Orders),
    maps:foreach(fun(ID, Courier) -> draw_courier(DC, ID, Courier) end, Couriers),
    draw_legend(DC),
    wxPaintDC:destroy(DC),
    ok.
draw_city(DC, City) ->
    draw_roads(DC, City#city.roads),
    draw_houses(DC, City#city.houses),
    draw_businesses(DC, City#city.businesses).
draw_roads(DC, Roads) ->
    wxDC:setPen(DC, wxPen:new(?COLOR_ROAD, [{style, ?wxSOLID}])),
    wxDC:setBrush(DC, wxBrush:new(?COLOR_ROAD)),
    lists:foreach(fun(#road{start_point={X1, Y1}, end_point={X2, Y2}, width=W}) ->
        if abs(X1 - X2) > abs(Y1 - Y2) -> TopY = Y1 - W div 2, wxDC:drawRectangle(DC, {min(X1, X2), TopY}, {abs(X2 - X1), W});
           true -> LeftX = X1 - W div 2, wxDC:drawRectangle(DC, {LeftX, min(Y1, Y2)}, {W, abs(Y2 - Y1)})
        end
    end, Roads).
draw_houses(DC, Houses) ->
    wxDC:setPen(DC, wxPen:new({0,0,0}, [{width, 1}])),
    wxDC:setBrush(DC, wxBrush:new(?COLOR_HOUSE)),
    lists:foreach(fun(#house{location={X, Y}}) -> wxDC:drawRectangle(DC, {X-6, Y-6}, {12, 12}) end, Houses).
draw_businesses(DC, Businesses) ->
    wxDC:setPen(DC, wxPen:new({0,0,0}, [{width, 2}])),
    wxDC:setBrush(DC, wxBrush:new(?COLOR_BUSINESS)),
    lists:foreach(fun(#business{location={X, Y}}) -> wxDC:drawRectangle(DC, {X-8, Y-8}, {16, 16}) end, Businesses).
draw_courier(DC, ID, CourierData) ->
    #{location := {X, Y}, status := Status} = CourierData,
    Color = case Status of available -> ?COLOR_COURIER_AVAILABLE; busy -> ?COLOR_COURIER_BUSY; _ -> {128, 128, 128} end,
    wxDC:setBrush(DC, wxBrush:new(Color)), wxDC:setPen(DC, wxPen:new({0, 0, 0}, [{width, 2}])),
    wxDC:drawCircle(DC, {round(X), round(Y)}, 10),
    IDStr = atom_to_list(ID), wxDC:drawText(DC, string:slice(IDStr, 8), {round(X) - 4, round(Y) - 8}).
draw_order(DC, Order) ->
    #{business_location := {BX, BY}, customer_location := {CX, CY}} = Order,
    wxDC:setBrush(DC, wxBrush:new(?COLOR_BUSINESS)), wxDC:setPen(DC, wxPen:new({0, 0, 0}, [{width, 2}])),
    wxDC:drawRectangle(DC, {round(BX) - 8, round(BY) - 8}, {16, 16}),
    wxDC:setBrush(DC, wxBrush:new(?COLOR_CUSTOMER)),
    Points = [{round(CX), round(CY) - 10}, {round(CX) - 8, round(CY) + 8}, {round(CX) + 8, round(CY) + 8}],
    wxDC:drawPolygon(DC, Points),
    wxDC:setPen(DC, wxPen:new({128, 128, 128}, [{width, 1}, {style, ?wxDOT}])),
    wxDC:drawLine(DC, {round(BX), round(BY)}, {round(CX), round(CY)}).
draw_legend(_DC) -> ok.

%% ===================================================================
%% Internal functions - Demo Logic
%% ===================================================================

%% תיקון: פונקציה חדשה לטיפול בהגעת שליח ליעד
handle_arrival(CourierID, CourierData, Orders) ->
    OrderID = maps:get(order_id, CourierData),
    OrderData = maps:get(OrderID, Orders),
    CurrentTarget = maps:get(target_location, CourierData),
    BusinessLoc = maps:get(business_location, OrderData),
    if
        CurrentTarget == BusinessLoc ->
            %% הגיע לעסק, שנה יעד ללקוח
            io:format("~p picked up order ~p~n", [CourierID, OrderID]),
            CustomerLoc = maps:get(customer_location, OrderData),
            CourierData#{target_location => CustomerLoc};
        true ->
            %% הגיע ללקוח, סיים את המשלוח
            io:format("~p delivered order ~p~n", [CourierID, OrderID]),
            gen_server:cast(?SERVER, {remove_order, OrderID}),
            CourierData#{status => available, target_location => undefined, order_id => undefined}
    end.

%% תיקון: פונקציה חדשה שמזיזה שליח צעד אחד לכיוון המטרה
move_towards({X, Y}, {TargetX, TargetY}) ->
    NewX = if X < TargetX -> X + ?COURIER_SPEED; X > TargetX -> X - ?COURIER_SPEED; true -> X end,
    NewY = if Y < TargetY -> Y + ?COURIER_SPEED; Y > TargetY -> Y - ?COURIER_SPEED; true -> Y end,
    %% ודא שלא עברנו את המטרה
    FinalX = if (X < TargetX andalso NewX > TargetX) orelse (X > TargetX andalso NewX < TargetX) -> TargetX; true -> NewX end,
    FinalY = if (Y < TargetY andalso NewY > TargetY) orelse (Y > TargetY andalso NewY < TargetY) -> TargetY; true -> NewY end,
    {FinalX, FinalY}.

%% תיקון: אתחול יעדים לשליחים עסוקים
init_demo_couriers(#city{roads = Roads}, Orders) ->
    Courier1Pos = city_generator:find_nearest_road_point({rand:uniform(?MAP_WIDTH), rand:uniform(?MAP_HEIGHT)}, Roads),
    Courier2Pos = city_generator:find_nearest_road_point({rand:uniform(?MAP_WIDTH), rand:uniform(?MAP_HEIGHT)}, Roads),
    Courier3Pos = city_generator:find_nearest_road_point({rand:uniform(?MAP_WIDTH), rand:uniform(?MAP_HEIGHT)}, Roads),
    
    %% שייך את ההזמנות לשליחים העסוקים
    [Order1, Order2 | _] = maps:values(Orders),
    Order1ID = maps:get(id, Order1),
    Order2ID = maps:get(id, Order2),
    Business1Loc = maps:get(business_location, Order1),
    
    #{
        courier_1 => #{location => Courier1Pos, status => available},
        courier_2 => #{location => Courier2Pos, status => busy, order_id => Order1ID, target_location => Business1Loc},
        courier_3 => #{location => Courier3Pos, status => busy, order_id => Order2ID, target_location => maps:get(business_location, Order2)}
    }.

init_demo_orders(#city{businesses = Businesses, houses = Houses}) ->
    case {Businesses, Houses} of
        {[_|_], [_|_]} ->
            B1 = lists:nth(rand:uniform(length(Businesses)), Businesses),
            H1 = lists:nth(rand:uniform(length(Houses)), Houses),
            B2 = lists:nth(rand:uniform(length(Businesses)), Businesses),
            H2 = lists:nth(rand:uniform(length(Houses)), Houses),
            #{
                order_1 => #{id => order_1, business_location => B1#business.location, customer_location => H1#house.location, status => in_progress},
                order_2 => #{id => order_2, business_location => B2#business.location, customer_location => H2#house.location, status => waiting}
            };
        _ -> #{}
    end.