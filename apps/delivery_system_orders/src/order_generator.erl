%%%-------------------------------------------------------------------
%% order_generator - gen_server responsible for creating new orders
%%      משתמש עכשיו ב-order_config לקונפיגורציה דינמית
%%      מעביר הזמנות דרך order_validator ו-order_repository
%%%-------------------------------------------------------------------
-module(order_generator).
-behaviour(gen_server).

-export([start_link/0]).
-export([start_generation/0, stop_generation/0, set_interval/1, get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    generation_active = true :: boolean(),      %% האם המחולל פעיל
    generation_interval :: integer(),            %% מרווח זמן בין הזמנות
    timer_ref :: reference() | undefined,        %% reference לטיימר הנוכחי
    order_counter = 0 :: integer(),             %% מספר הזמנות שנוצרו
    success_counter = 0 :: integer(),           %% מספר הזמנות שעברו ולידציה
    failed_counter = 0 :: integer(),            %% מספר הזמנות שנכשלו
    last_order_time :: integer() | undefined    %% זמן יצירת הזמנה אחרונה
}).


%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% הפעלת מחולל ההזמנות
start_generation() ->
    gen_server:cast(?SERVER, start_generation).

%% עצירת מחולל ההזמנות
stop_generation() ->
    gen_server:cast(?SERVER, stop_generation).

%% הגדרת מרווח זמן חדש
set_interval(IntervalMs) ->
    gen_server:call(?SERVER, {set_interval, IntervalMs}).

%% קבלת סטטיסטיקות המחולל
get_stats() ->
    gen_server:call(?SERVER, get_stats).


%% gen_server Callbacks
init([]) ->
    %% טעינת קונפיגורציה מ-order_config
    Interval = order_config:get_generation_interval(),
    
    io:format("Order generator started with interval ~p ms.~n", [Interval]),
    
    %% התחלת יצירה אוטומטית של הזמנות
    TimerRef = erlang:send_after(1000, self(), generate_order),
    
    {ok, #state{
        generation_active = true,
        generation_interval = Interval,
        timer_ref = TimerRef,
        last_order_time = erlang:system_time(millisecond)
    }}.


handle_call({set_interval, IntervalMs}, _From, State) ->
    %% עדכון המרווח בקונפיגורציה
    order_config:set_generation_interval(IntervalMs),
    
    io:format("Order generator: Interval changed to ~p ms.~n", [IntervalMs]),
    
    {reply, ok, State#state{generation_interval = IntervalMs}};

handle_call(get_stats, _From, State) ->
    Stats = #{
        active => State#state.generation_active,
        interval_ms => State#state.generation_interval,
        total_generated => State#state.order_counter,
        successful => State#state.success_counter,
        failed => State#state.failed_counter,
        success_rate => calculate_success_rate(State),
        last_order_time => State#state.last_order_time
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


handle_cast(start_generation, State = #state{generation_active = false}) ->
    %% הפעלת המחולל מחדש
    TimerRef = erlang:send_after(State#state.generation_interval, self(), generate_order),
    
    io:format("Order generator: Generation started.~n"),
    
    {noreply, State#state{
        generation_active = true,
        timer_ref = TimerRef
    }};

handle_cast(start_generation, State) ->
    %% כבר פעיל
    {noreply, State};

handle_cast(stop_generation, State = #state{timer_ref = TimerRef}) ->
    %% ביטול הטיימר אם קיים
    case TimerRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TimerRef)
    end,
    
    io:format("Order generator: Generation stopped.~n"),
    
    {noreply, State#state{
        generation_active = false,
        timer_ref = undefined
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% Generate a new order
handle_info(generate_order, State = #state{generation_active = false}) ->
    %% המחולל לא פעיל, לא יוצרים הזמנה
    {noreply, State};

handle_info(generate_order, State = #state{order_counter = Counter}) ->
    NewCounter = Counter + 1,
    
    %% יצירת הזמנה חדשה עם נתונים מהקונפיגורציה
    OrderData = create_order_data(),
    
    io:format("Order generator: Creating order ~p~n", [maps:get(id, OrderData)]),
    
    %% שליחת ההזמנה לולידציה
    ValidationResult = order_validator:validate_order(OrderData),
    
    NewState = case ValidationResult of
        {ok, ValidatedOrder} ->
            %% הזמנה עברה ולידציה - שמירה ושליחה לתור
            process_validated_order(ValidatedOrder),
            
            State#state{
                order_counter = NewCounter,
                success_counter = State#state.success_counter + 1,
                last_order_time = erlang:system_time(millisecond)
            };
        
        {error, Reason} ->
            io:format("Order generator: Order validation failed: ~p~n", [Reason]),
            
            State#state{
                order_counter = NewCounter,
                failed_counter = State#state.failed_counter + 1
            }
    end,
    
    %% תזמון ההזמנה הבאה
    TimerRef = erlang:send_after(State#state.generation_interval, self(), generate_order),
    
    {noreply, NewState#state{timer_ref = TimerRef}};

%% טיפול בעדכוני קונפיגורציה
handle_info({config_update, interval, NewInterval}, State) ->
    io:format("Order generator: Received config update - interval: ~p ms~n", [NewInterval]),
    
    %% ביטול הטיימר הנוכחי אם קיים
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    
    %% תזמון מחדש עם המרווח החדש
    TimerRef = erlang:send_after(NewInterval, self(), generate_order),
    
    {noreply, State#state{
        generation_interval = NewInterval,
        timer_ref = TimerRef
    }};

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, State) ->
    %% ביטול הטיימר אם קיים
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    
    %% שמירת סטטיסטיקות סופיות
    io:format("Order generator terminated. Total orders: ~p, Success: ~p, Failed: ~p~n",
              [State#state.order_counter, State#state.success_counter, State#state.failed_counter]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ===================================================================
%% פונקציות עזר פנימיות
%% ===================================================================

%% יצירת נתוני הזמנה
create_order_data() ->
    %% יצירת ID ייחודי
    OrderID = erlang:unique_integer([positive]),
    
    %% בחירת סוג עסק וקבלת מיקומו
    {BusinessType, BusinessLocation} = select_business(),
    
    %% יצירת מיקום לקוח
    CustomerLocation = generate_customer_location(),
    
    %% יצירת נתוני הזמנה
    #{
        id => OrderID,
        business_type => BusinessType,
        business_location => BusinessLocation,
        customer_location => CustomerLocation,
        created_at => erlang:system_time(millisecond),
        source => generator,
        priority => select_priority(),
        items => generate_order_items(BusinessType),
        estimated_value => generate_order_value()
    }.

%% בחירת עסק על פי משקלים מהקונפיגורציה
select_business() ->
    BusinessTypes = order_config:get_business_types(),
    
    %% אם יש עסקים מוגדרים, בחר אחד
    case maps:to_list(BusinessTypes) of
        [] ->
            %% אין עסקים מוגדרים - יצירת ברירת מחדל
            {default, generate_random_location()};
        TypesList ->
            %% בחירה אקראית או לפי משקלים
            case order_config:get_config(business_weights) of
                undefined ->
                    %% בחירה אקראית פשוטה
                    {Type, Location} = lists:nth(rand:uniform(length(TypesList)), TypesList),
                    {Type, add_noise_to_location(Location)};
                Weights ->
                    %% בחירה לפי משקלים
                    select_weighted_business(TypesList, Weights)
            end
    end.

%% בחירת עסק לפי משקלים
select_weighted_business(TypesList, Weights) ->
    %% חישוב משקל כולל
    TotalWeight = lists:sum([maps:get(Type, Weights, 0) || {Type, _} <- TypesList]),
    
    %% בחירה אקראית משוקללת
    Random = rand:uniform() * TotalWeight,
    select_by_weight(TypesList, Weights, Random, 0).

select_by_weight([{Type, Location} | Rest], Weights, Random, Accumulated) ->
    Weight = maps:get(Type, Weights, 0),
    NewAccumulated = Accumulated + Weight,
    if
        Random =< NewAccumulated ->
            {Type, add_noise_to_location(Location)};
        true ->
            select_by_weight(Rest, Weights, Random, NewAccumulated)
    end;
select_by_weight([], _, _, _) ->
    %% ברירת מחדל אם משהו השתבש
    {default, generate_random_location()}.

%% הוספת רעש קטן למיקום כדי למנוע מיקומים זהים
add_noise_to_location({X, Y}) ->
    NoiseX = (rand:uniform() - 0.5) * 2,  % רעש בין -1 ל-1
    NoiseY = (rand:uniform() - 0.5) * 2,
    {X + NoiseX, Y + NoiseY}.

%% יצירת מיקום לקוח
generate_customer_location() ->
    %% שימוש בהתפלגות לקוחות מהקונפיגורציה
    case order_config:get_config(customer_distribution) of
        undefined ->
            generate_random_location();
        Distribution ->
            generate_distributed_location(Distribution)
    end.

%% יצירת מיקום לפי התפלגות
generate_distributed_location(Distribution) ->
    %% בחירת אזור לפי ההתפלגות
    Zone = select_zone(Distribution),
    
    %% יצירת מיקום באזור המתאים
    generate_location_in_zone(Zone).

%% בחירת אזור לפי התפלגות
select_zone(Distribution) ->
    Random = rand:uniform(),
    select_zone_by_probability(maps:to_list(Distribution), Random, 0).

select_zone_by_probability([{Zone, Prob} | Rest], Random, Accumulated) ->
    NewAccumulated = Accumulated + Prob,
    if
        Random =< NewAccumulated -> Zone;
        true -> select_zone_by_probability(Rest, Random, NewAccumulated)
    end;
select_zone_by_probability([], _, _) ->
    residential. % ברירת מחדל

%% יצירת מיקום באזור ספציפי
generate_location_in_zone(residential) ->
    %% אזורי מגורים - בדרך כלל בפריפריה
    {MinX, MinY} = {20, 20},
    {MaxX, MaxY} = {80, 80},
    {MinX + rand:uniform() * (MaxX - MinX), 
     MinY + rand:uniform() * (MaxY - MinY)};

generate_location_in_zone(commercial) ->
    %% אזורי מסחר - במרכז
    {MinX, MinY} = {40, 40},
    {MaxX, MaxY} = {60, 60},
    {MinX + rand:uniform() * (MaxX - MinX), 
     MinY + rand:uniform() * (MaxY - MinY)};

generate_location_in_zone(industrial) ->
    %% אזורי תעשייה - בקצוות
    case rand:uniform(4) of
        1 -> {rand:uniform() * 20, rand:uniform() * 100};          % שמאל
        2 -> {80 + rand:uniform() * 20, rand:uniform() * 100};     % ימין
        3 -> {rand:uniform() * 100, rand:uniform() * 20};          % למטה
        4 -> {rand:uniform() * 100, 80 + rand:uniform() * 20}      % למעלה
    end.

%% יצירת מיקום אקראי בטווח המוגדר
generate_random_location() ->
    {MinLoc, MaxLoc} = order_config:get_location_range(),
    {MinX, MinY} = MinLoc,
    {MaxX, MaxY} = MaxLoc,
    {
        MinX + rand:uniform() * (MaxX - MinX),
        MinY + rand:uniform() * (MaxY - MinY)
    }.

%% בחירת עדיפות להזמנה
select_priority() ->
    %% 10% דחוף, 30% גבוה, 60% רגיל
    Random = rand:uniform(),
    if
        Random < 0.1 -> urgent;
        Random < 0.4 -> high;
        true -> normal
    end.

%% יצירת פריטים להזמנה
generate_order_items(BusinessType) ->
    %% מספר פריטים אקראי בין 1-5
    NumItems = rand:uniform(5),
    [generate_item(BusinessType, I) || I <- lists:seq(1, NumItems)].

generate_item(restaurant, _) ->
    #{name => "Food item", quantity => rand:uniform(3), price => 10 + rand:uniform(40)};
generate_item(pharmacy, _) ->
    #{name => "Medicine", quantity => 1, price => 5 + rand:uniform(50)};
generate_item(grocery, _) ->
    #{name => "Grocery item", quantity => rand:uniform(10), price => 2 + rand:uniform(20)};
generate_item(_, _) ->
    #{name => "Item", quantity => rand:uniform(3), price => 10 + rand:uniform(100)}.

%% יצירת ערך להזמנה
generate_order_value() ->
    20 + rand:uniform(180). % ערך בין 20 ל-200

%% עיבוד הזמנה שעברה ולידציה
process_validated_order(ValidatedOrder) ->
    %% שמירה ב-repository
    order_repository:store_order(ValidatedOrder),
    
    %% שליחה לתור של מנהל השליחים
    courier_manager:add_to_queue(ValidatedOrder),
    
    %% עדכון אנליטיקה
    order_analytics:update_order_completed(ValidatedOrder#{status => queued}),
    
    io:format("Order generator: Order ~p validated and queued successfully.~n", 
              [maps:get(id, ValidatedOrder)]).

%% חישוב שיעור הצלחה
calculate_success_rate(#state{order_counter = 0}) ->
    100.0;
calculate_success_rate(#state{order_counter = Total, success_counter = Success}) ->
    (Success / Total) * 100.