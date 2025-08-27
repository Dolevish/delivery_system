%%%-------------------------------------------------------------------
%% @doc order_validator - מודול לבדיקת תקינות הזמנות
%%      בודק שהזמנות מכילות את כל השדות הנדרשים ושהם תקינים
%%      מוודא שמיקומים בטווח מוגדר ושיש ID ייחודי
%% @end
%%%-------------------------------------------------------------------
-module(order_validator).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([validate_order/1, set_validation_rules/1, get_validation_rules/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    min_location :: {number(), number()},  %% מיקום מינימלי
    max_location :: {number(), number()},  %% מיקום מקסימלי
    required_fields :: [atom()],           %% שדות חובה
    validated_ids :: gb_sets:set()         %% IDs שכבר נבדקו
}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% בדיקת תקינות הזמנה
validate_order(OrderData) ->
    gen_server:call(?SERVER, {validate, OrderData}).

%% הגדרת חוקי ולידציה
set_validation_rules(Rules) ->
    gen_server:call(?SERVER, {set_rules, Rules}).

%% קבלת חוקי ולידציה נוכחיים
get_validation_rules() ->
    gen_server:call(?SERVER, get_rules).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %% טעינת חוקי ולידציה ברירת מחדל מהקונפיגורציה
    {MinLoc, MaxLoc} = order_config:get_location_range(),
    RequiredFields = [id, business_location, customer_location],
    
    io:format("Order validator started with location range ~p to ~p.~n", 
              [MinLoc, MaxLoc]),
    
    {ok, #state{
        min_location = MinLoc,
        max_location = MaxLoc,
        required_fields = RequiredFields,
        validated_ids = gb_sets:new()
    }}.

handle_call({validate, OrderData}, _From, State) ->
    %% ביצוע כל הבדיקות על ההזמנה
    ValidationResult = perform_validation(OrderData, State),
    
    %% במקרה של הצלחה, שמירת ה-ID כדי למנוע כפילויות
    NewState = case ValidationResult of
        {ok, ValidatedOrder} ->
            OrderID = maps:get(id, ValidatedOrder),
            State#state{validated_ids = gb_sets:add(OrderID, State#state.validated_ids)};
        _ ->
            State
    end,
    
    {reply, ValidationResult, NewState};

handle_call({set_rules, Rules}, _From, State) ->
    %% עדכון חוקי ולידציה
    NewState = State#state{
        min_location = maps:get(min_location, Rules, State#state.min_location),
        max_location = maps:get(max_location, Rules, State#state.max_location),
        required_fields = maps:get(required_fields, Rules, State#state.required_fields)
    },
    {reply, ok, NewState};

handle_call(get_rules, _From, State) ->
    Rules = #{
        min_location => State#state.min_location,
        max_location => State#state.max_location,
        required_fields => State#state.required_fields
    },
    {reply, Rules, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% פונקציות ולידציה פנימיות
%% ===================================================================

%% ביצוע כל הבדיקות על ההזמנה
perform_validation(OrderData, State) ->
    try
        %% בדיקת שדות חובה
        ok = validate_required_fields(OrderData, State#state.required_fields),
        
        %% בדיקת ID ייחודי
        OrderID = maps:get(id, OrderData),
        ok = validate_unique_id(OrderID, State#state.validated_ids),
        
        %% בדיקת תקינות מיקומים
        BusinessLoc = maps:get(business_location, OrderData),
        CustomerLoc = maps:get(customer_location, OrderData),
        ok = validate_location(BusinessLoc, State#state.min_location, State#state.max_location),
        ok = validate_location(CustomerLoc, State#state.min_location, State#state.max_location),
        
        %% בדיקות נוספות
        ok = validate_order_data_types(OrderData),
        
        %% הוספת מידע נוסף להזמנה המאומתת
        ValidatedOrder = OrderData#{
            validated => true,
            validated_at => erlang:system_time(millisecond)
        },
        
        {ok, ValidatedOrder}
    catch
        throw:{validation_error, Reason} ->
            {error, Reason};
        _:Error ->
            {error, {unexpected_error, Error}}
    end.

%% בדיקת שדות חובה
validate_required_fields(OrderData, RequiredFields) ->
    Missing = lists:filter(fun(Field) -> 
        not maps:is_key(Field, OrderData)
    end, RequiredFields),
    
    case Missing of
        [] -> ok;
        _ -> throw({validation_error, {missing_fields, Missing}})
    end.

%% בדיקת ID ייחודי
validate_unique_id(OrderID, ValidatedIDs) ->
    case gb_sets:is_member(OrderID, ValidatedIDs) of
        true ->
            %% בדיקה נוספת במאגר ההזמנות
            case order_repository:get_order(OrderID) of
                {ok, _} ->
                    throw({validation_error, {duplicate_id, OrderID}});
                {error, not_found} ->
                    ok
            end;
        false ->
            ok
    end.

%% בדיקת תקינות מיקום
validate_location({X, Y}, {MinX, MinY}, {MaxX, MaxY}) when is_number(X), is_number(Y) ->
    if
        X < MinX orelse X > MaxX ->
            throw({validation_error, {location_out_of_range, x, X, {MinX, MaxX}}});
        Y < MinY orelse Y > MaxY ->
            throw({validation_error, {location_out_of_range, y, Y, {MinY, MaxY}}});
        true ->
            ok
    end;
validate_location(Location, _, _) ->
    throw({validation_error, {invalid_location_format, Location}}).

%% בדיקת סוגי נתונים
validate_order_data_types(OrderData) ->
    %% בדיקה ש-ID הוא מספר או אטום
    ID = maps:get(id, OrderData),
    case is_integer(ID) orelse is_atom(ID) of
        true -> ok;
        false -> throw({validation_error, {invalid_id_type, ID}})
    end,
    
    %% בדיקת שדות אופציונליים
    validate_optional_fields(OrderData).

%% בדיקת שדות אופציונליים
validate_optional_fields(OrderData) ->
    %% בדיקת סוג עסק אם קיים
    case maps:get(business_type, OrderData, undefined) of
        undefined -> ok;
        Type when is_atom(Type) -> ok;
        InvalidType -> throw({validation_error, {invalid_business_type, InvalidType}})
    end,
    
    %% בדיקת פרטי לקוח אם קיימים
    case maps:get(customer_info, OrderData, undefined) of
        undefined -> ok;
        Info when is_map(Info) -> ok;
        InvalidInfo -> throw({validation_error, {invalid_customer_info, InvalidInfo}})
    end,
    
    ok.