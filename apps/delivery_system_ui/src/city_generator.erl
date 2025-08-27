%%%-------------------------------------------------------------------
%% @doc city_generator - מחולל עיר עם בתים, בתי עסק וכבישים
%%      יוצר פריסה ריאליסטית של עיר עם רחובות ומבנים
%% @end
%%%-------------------------------------------------------------------
-module(city_generator).

-export([generate_city/0, generate_city/1, get_city_data/0]).
-export([find_nearest_road_point/2, is_valid_location/2]).

%% מבני נתונים
-include("city_generator.hrl").


%% ===================================================================
%% API
%% ===================================================================

%% יצירת עיר עם הגדרות ברירת מחדל
generate_city() ->
    generate_city(#{
        width => 800,
        height => 600,
        num_houses => 100,
        num_businesses => 5
    }).

%% יצירת עיר עם הגדרות מותאמות
generate_city(Config) ->
    Width = maps:get(width, Config, 800),
    Height = maps:get(height, Config, 600),
    NumHouses = maps:get(num_houses, Config, 100),
    NumBusinesses = maps:get(num_businesses, Config, 5),

    io:format("~nיוצר עיר חדשה...~n"),
    io:format("גודל: ~px~p, בתים: ~p, בתי עסק: ~p~n",
              [Width, Height, NumHouses, NumBusinesses]),

    %% שלב 1: יצירת רשת כבישים
    {Roads, Intersections} = generate_road_network(Width, Height),
    io:format("✓ נוצרו ~p כבישים ו-~p צמתים~n",
              [length(Roads), length(Intersections)]),

    %% שלב 2: יצירת אזורי מגורים ומסחר
    Zones = create_zones(Width, Height),

    %% שלב 3: פיזור בתי עסק
    Businesses = place_businesses(NumBusinesses, Roads, Zones, Width, Height),
    io:format("✓ מוקמו ~p בתי עסק~n", [length(Businesses)]),

    %% שלב 4: פיזור בתים
    Houses = place_houses(NumHouses, Roads, Businesses, Zones, Width, Height),
    io:format("✓ נבנו ~p בתים~n", [length(Houses)]),

    %% הרכבת מבנה העיר
    City = #city{
        roads = Roads,
        houses = Houses,
        businesses = Businesses,
        intersections = Intersections,
        grid_size = {Width, Height}
    },

    %% שמירת הנתונים ב-ETS לגישה גלובלית
    save_city_data(City),

    io:format("✓ העיר נוצרה בהצלחה!~n~n"),

    City.


%% קבלת נתוני העיר השמורים
get_city_data() ->
    case ets:info(city_data) of
        undefined ->
            generate_city(), % צור עיר אם אין
            get_city_data();
        _ ->
            case ets:lookup(city_data, current_city) of
                [{current_city, City}] -> City;
                [] ->
                    generate_city(),
                    get_city_data()
            end
    end.

%% ===================================================================
%% Road Network Generation
%% ===================================================================

%% יצירת רשת כבישים ריאליסטית
generate_road_network(Width, Height) ->
    %% רחובות ראשיים - צירים מרכזיים
    MainRoads = [
        %% כביש ראשי אופקי במרכז
        #road{
            id = main_horizontal_1,
            type = main,
            start_point = {0, Height div 2},
            end_point = {Width, Height div 2},
            width = 40
        },
        %% כביש ראשי נוסף בצפון
        #road{
            id = main_horizontal_2,
            type = main,
            start_point = {0, Height div 4},
            end_point = {Width, Height div 4},
            width = 40
        },
        %% כביש ראשי אנכי במרכז
        #road{
            id = main_vertical_1,
            type = main,
            start_point = {Width div 2, 0},
            end_point = {Width div 2, Height},
            width = 40
        },
        %% כביש ראשי אנכי במערב
        #road{
            id = main_vertical_2,
            type = main,
            start_point = {Width div 4, 0},
            end_point = {Width div 4, Height},
            width = 40
        },
        %% כביש ראשי אנכי במזרח
        #road{
            id = main_vertical_3,
            type = main,
            start_point = {3 * Width div 4, 0},
            end_point = {3 * Width div 4, Height},
            width = 40
        }
    ],

    %% רחובות משניים - יוצרים רשת
    SecondaryRoads = generate_secondary_roads(Width, Height),

    %% רחובות מגורים - רחובות קטנים בשכונות
    ResidentialRoads = generate_residential_roads(Width, Height),

    AllRoads = MainRoads ++ SecondaryRoads ++ ResidentialRoads,

    %% חישוב צמתים
    Intersections = calculate_intersections(AllRoads),

    {AllRoads, Intersections}.

%% יצירת רחובות משניים
generate_secondary_roads(Width, Height) ->
    %% רחובות אופקיים משניים
    HorizontalSecondary = [
        #road{
            id = list_to_atom("sec_h_" ++ integer_to_list(Y)),
            type = secondary,
            start_point = {0, Y},
            end_point = {Width, Y},
            width = 25
        } || Y <- [Height div 8, 3 * Height div 8, 5 * Height div 8, 7 * Height div 8]
    ],

    %% רחובות אנכיים משניים
    VerticalSecondary = [
        #road{
            id = list_to_atom("sec_v_" ++ integer_to_list(X)),
            type = secondary,
            start_point = {X, 0},
            end_point = {X, Height},
            width = 25
        } || X <- [Width div 8, 3 * Width div 8, 5 * Width div 8, 7 * Width div 8]
    ],

    HorizontalSecondary ++ VerticalSecondary.

%% יצירת רחובות מגורים קטנים
generate_residential_roads(Width, Height) ->
    %% יוצרים רחובות קטנים בתוך כל "בלוק" שנוצר מהרחובות הגדולים
    ResRoads = lists:flatten([
        generate_block_roads(
            {(X-1) * Width div 8, (Y-1) * Height div 8},
            {X * Width div 8, Y * Height div 8},
            {X, Y}
        ) || X <- [1,2,3,4,5,6,7,8], Y <- [1,2,3,4,5,6,7,8]
    ]),

    %% מסננים רחובות קצרים מדי
    [R || R <- ResRoads, road_length(R) > 30].

%% יצירת רחובות בתוך בלוק בודד
generate_block_roads(TopLeft, BottomRight, BlockID) ->
    {X1, Y1} = TopLeft,
    {X2, Y2} = BottomRight,
    BlockWidth = X2 - X1,
    BlockHeight = Y2 - Y1,

    %% רק אם הבלוק גדול מספיק
    if
        BlockWidth > 60, BlockHeight > 60 ->
            %% התיקון כאן - יצירת שם תקין לפני ההמרה לאטום
            {BlockX, BlockY} = BlockID,
            RoadIDStr = "res_" ++ integer_to_list(BlockX) ++ "_" ++ integer_to_list(BlockY) ++ "_mid",

            %% רחוב אופקי במרכז הבלוק
            MidRoad = #road{
                id = list_to_atom(RoadIDStr),
                type = residential,
                start_point = {X1 + 15, Y1 + BlockHeight div 2},
                end_point = {X2 - 15, Y1 + BlockHeight div 2},
                width = 15
            },
            [MidRoad];
        true ->
            []
    end.


%% חישוב אורך כביש
road_length(#road{start_point = {X1, Y1}, end_point = {X2, Y2}}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

%% חישוב צמתים
calculate_intersections(Roads) ->
    %% מצא את כל נקודות החיתוך בין כבישים
    Intersections = lists:flatten([
        find_intersection(R1, R2) ||
        R1 <- Roads,
        R2 <- Roads,
        R1#road.id < R2#road.id  % למנוע כפילויות
    ]),

    %% מסנן nulls
    [I || I <- Intersections, I =/= null].

%% מציאת נקודת חיתוך בין שני כבישים
find_intersection(Road1, Road2) ->
    {X1a, Y1a} = Road1#road.start_point,
    {X1b, Y1b} = Road1#road.end_point,
    {X2a, Y2a} = Road2#road.start_point,
    {X2b, Y2b} = Road2#road.end_point,

    %% בדיקה פשוטה - אם אחד אופקי והשני אנכי
    IsHorizontal1 = abs(Y1b - Y1a) < 5,
    IsVertical1 = abs(X1b - X1a) < 5,
    IsHorizontal2 = abs(Y2b - Y2a) < 5,
    IsVertical2 = abs(X2b - X2a) < 5,

    if
        IsHorizontal1 andalso IsVertical2 ->
            %% Road1 אופקי, Road2 אנכי
            X = (X2a + X2b) div 2,
            Y = (Y1a + Y1b) div 2,
            %% בדוק אם הנקודה בתחום שני הכבישים
            case {X >= min(X1a, X1b), X =< max(X1a, X1b), Y >= min(Y2a, Y2b), Y =< max(Y2a, Y2b)} of
                {true, true, true, true} ->
                    {X, Y};
                _ ->
                    null
            end;
        IsVertical1 andalso IsHorizontal2 ->
            %% Road1 אנכי, Road2 אופקי
            X = (X1a + X1b) div 2,
            Y = (Y2a + Y2b) div 2,
            %% בדוק אם הנקודה בתחום שני הכבישים
            case {X >= min(X2a, X2b), X =< max(X2a, X2b), Y >= min(Y1a, Y1b), Y =< max(Y1a, Y1b)} of
                {true, true, true, true} ->
                    {X, Y};
                _ ->
                    null
            end;
        true ->
            null
    end.

%% ===================================================================
%% Zone Creation
%% ===================================================================

%% יצירת אזורי עיר (מגורים, מסחר, תעשייה)
create_zones(Width, Height) ->
    #{
        %% מרכז העיר - אזור מסחרי
        commercial => [
            {Width div 2 - 100, Height div 2 - 100, 200, 200}
        ],
        %% שכונות מגורים
        residential => [
            {50, 50, Width div 3, Height div 3},           % שכונה צפון-מערב
            {Width - Width div 3 - 50, 50, Width div 3, Height div 3}, % צפון-מזרח
            {50, Height - Height div 3 - 50, Width div 3, Height div 3}, % דרום-מערב
            {Width - Width div 3 - 50, Height - Height div 3 - 50, Width div 3, Height div 3} % דרום-מזרח
        ],
        %% אזור תעשייה קטן
        industrial => [
            {Width - 150, Height - 100, 100, 80}
        ]
    }.

%% ===================================================================
%% Business Placement
%% ===================================================================

%% מיקום בתי עסק
place_businesses(NumBusinesses, Roads, Zones, Width, Height) ->
    %% סוגי עסקים שונים
    BusinessTypes = [
        {supermarket, "סופרמרקט מרכזי", large},
        {gas_station, "תחנת דלק", medium},
        {restaurant, "מסעדת השכונה", small},
        {pharmacy, "בית מרקחת", small},
        {bank, "סניף בנק", medium}
    ],

    %% מקם עסקים לאורך כבישים ראשיים ובאזורים מסחריים
    MainRoads = [R || R <- Roads, R#road.type == main],
    CommercialZones = maps:get(commercial, Zones, []),

    {Businesses, _} = lists:mapfoldl(fun(Index, UsedLocations) ->
        {Type, Name, Size} = lists:nth((Index rem length(BusinessTypes)) + 1, BusinessTypes),

        %% מצא מיקום מתאים
        Location = find_business_location(
            MainRoads,
            CommercialZones,
            UsedLocations,
            Width,
            Height
        ),

        Business = #business{
            id = list_to_atom("business_" ++ integer_to_list(Index)),
            location = Location,
            name = Name ++ " " ++ integer_to_list(Index),
            type = Type,
            size = Size
        },

        {Business, [Location | UsedLocations]}
    end, [], lists:seq(1, NumBusinesses)),

    Businesses.

%% מציאת מיקום מתאים לבית עסק
find_business_location(Roads, CommercialZones, UsedLocations, Width, Height) ->
    %% נסה קודם באזור מסחרי
    Attempts = case CommercialZones of
        [{CX, CY, CW, CH}|_] ->
            %% נקודות אקראיות באזור המסחרי
            [{rand:uniform(CW) + CX, rand:uniform(CH) + CY} || _ <- lists:seq(1, 10)];
        [] ->
            []
    end,

    %% אם לא מצאנו באזור מסחרי, נסה ליד כביש ראשי
    AllAttempts = Attempts ++ generate_roadside_locations(Roads, 20),

    %% מצא מיקום שלא תפוס
    find_valid_location(AllAttempts, UsedLocations, 50, {Width div 2, Height div 2}).

%% יצירת מיקומים לצד הכביש
generate_roadside_locations(Roads, Count) ->
    lists:flatten([
        begin
            Road = lists:nth(rand:uniform(length(Roads)), Roads),
            {X1, Y1} = Road#road.start_point,
            {X2, Y2} = Road#road.end_point,

            %% נקודה אקראית לאורך הכביש
            T = rand:uniform(),
            X = round(X1 + T * (X2 - X1)),
            Y = round(Y1 + T * (Y2 - Y1)),

            %% הזז מעט הצידה מהכביש
            Offset = Road#road.width div 2 + 20,
            [
                {X + Offset, Y},
                {X - Offset, Y},
                {X, Y + Offset},
                {X, Y - Offset}
            ]
        end || _ <- lists:seq(1, Count div 4)
    ]).

%% ===================================================================
%% House Placement
%% ===================================================================

%% מיקום בתים
place_houses(NumHouses, Roads, Businesses, Zones, Width, Height) ->
    %% אסוף את כל המיקומים התפוסים
    BusinessLocations = [B#business.location || B <- Businesses],

    %% מיין כבישי מגורים
    ResidentialRoads = [R || R <- Roads, R#road.type == residential orelse R#road.type == secondary],
    ResidentialZones = maps:get(residential, Zones, []),

    %% מקם בתים
    {Houses, _} = lists:mapfoldl(fun(Index, UsedLocations) ->
        %% מצא מיקום מתאים
        Location = find_house_location(
            ResidentialRoads,
            ResidentialZones,
            UsedLocations ++ BusinessLocations,
            Width,
            Height
        ),

        %% גודל אקראי לבית
        Size = case rand:uniform(3) of
            1 -> small;
            2 -> medium;
            3 -> large
        end,

        House = #house{
            id = list_to_atom("house_" ++ integer_to_list(Index)),
            location = Location,
            address = generate_address(Location, Index),
            size = Size
        },

        {House, [Location | UsedLocations]}
    end, [], lists:seq(1, NumHouses)),

    Houses.

%% מציאת מיקום מתאים לבית
find_house_location(Roads, ResidentialZones, UsedLocations, Width, Height) ->
    %% יצירת רשת של מיקומים אפשריים בשכונות מגורים
    GridPoints = generate_residential_grid(ResidentialZones),

    %% הוסף נקודות לאורך כבישי מגורים
    RoadPoints = generate_roadside_locations(Roads, 50),

    AllPoints = GridPoints ++ RoadPoints,

    %% מצא מיקום פנוי
    find_valid_location(AllPoints, UsedLocations, 25, {Width div 2, Height div 2}).

%% יצירת רשת נקודות בשכונת מגורים
generate_residential_grid([{X, Y, W, H}|T]) ->
    %% יצירת רשת אחידה בתוך האזור
    GridSpacing = 35,  % מרחק בין בתים
    XPoints = lists:seq(X + 20, X + W - 20, GridSpacing),
    YPoints = lists:seq(Y + 20, Y + H - 20, GridSpacing),

    %% הוסף רעש קטן למיקום כל בית
    CurrentZonePoints = [{XP + rand:uniform(10) - 5, YP + rand:uniform(10) - 5} ||
     XP <- XPoints, YP <- YPoints],
    CurrentZonePoints ++ generate_residential_grid(T);
generate_residential_grid([]) ->
    [].


%% מציאת מיקום תקף (לא חופף)
find_valid_location([Point|Rest], UsedLocations, MinDistance, Default) ->
    case is_location_valid(Point, UsedLocations, MinDistance) of
        true -> Point;
        false -> find_valid_location(Rest, UsedLocations, MinDistance, Default)
    end;
find_valid_location([], _UsedLocations, _MinDistance, Default) ->
    Default. % אם לא נמצא מיקום, החזר ברירת מחדל

%% בדיקה אם מיקום תקף (לא קרוב מדי למיקומים אחרים)
is_location_valid({X, Y}, UsedLocations, MinDistance) ->
    not lists:any(fun({UX, UY}) ->
        Distance = math:sqrt(math:pow(X - UX, 2) + math:pow(Y - UY, 2)),
        Distance < MinDistance
    end, UsedLocations).

%% יצירת כתובת לבית
generate_address({X, Y}, Index) ->
    Streets = ["רחוב הדקלים", "שדרות הפרחים", "רחוב השקמים", "דרך הים", "רחוב הזית"],
    StreetName = lists:nth((Index rem length(Streets)) + 1, Streets),
    HouseNumber = integer_to_list((Index rem 200) + 1),
    StreetName ++ " " ++ HouseNumber.

%% ===================================================================
%% Data Storage
%% ===================================================================

%% שמירת נתוני העיר ב-ETS
save_city_data(City) ->
    %% צור טבלה אם לא קיימת
    case ets:info(city_data) of
        undefined ->
            ets:new(city_data, [named_table, public, set]);
        _ ->
            ok
    end,

    %% שמור את העיר
    ets:insert(city_data, {current_city, City}),
    ok.

%% ===================================================================
%% Helper Functions
%% ===================================================================

%% מצא את הנקודה הקרובה ביותר על הכביש
find_nearest_road_point({X, Y}, Roads) ->
    %% מצא את הכביש הקרוב ביותר והנקודה עליו
    {_MinDist, NearestPoint} = lists:foldl(fun(Road, {MinDist, BestPoint}) ->
        {RoadPoint, Dist} = point_to_road_distance({X, Y}, Road),
        if
            Dist < MinDist -> {Dist, RoadPoint};
            true -> {MinDist, BestPoint}
        end
    end, {999999, {X, Y}}, Roads),

    NearestPoint.

%% מרחק מנקודה לכביש
point_to_road_distance({PX, PY}, Road) ->
    {X1, Y1} = Road#road.start_point,
    {X2, Y2} = Road#road.end_point,

    %% חשב את הנקודה הקרובה ביותר על הקו
    L2 = math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2),
    if
        L2 == 0 -> {{X1, Y1}, math:sqrt(math:pow(PX - X1, 2) + math:pow(PY - Y1, 2))};
        true ->
            T = max(0, min(1, ((PX - X1) * (X2 - X1) + (PY - Y1) * (Y2 - Y1)) / L2)),
            ProjectionX = X1 + T * (X2 - X1),
            ProjectionY = Y1 + T * (Y2 - Y1),
            Distance = math:sqrt(math:pow(PX - ProjectionX, 2) + math:pow(PY - ProjectionY, 2)),
            {{round(ProjectionX), round(ProjectionY)}, Distance}
    end.

%% בדיקה אם מיקום תקף (מחוץ לכבישים)
is_valid_location({X, Y}, Roads) ->
    %% בדוק שהמיקום לא על כביש
    not lists:any(fun(Road) ->
        {_Point, Distance} = point_to_road_distance({X, Y}, Road),
        Distance < (Road#road.width div 2 + 5)
    end, Roads).