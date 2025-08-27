%%%-------------------------------------------------------------------
%% @doc order_api - מודול לקבלת הזמנות מממשקים חיצוניים
%%      תומך ב-REST API, message queue ופרוטוקולים נוספים
%%      מבצע validation מלא ומתעד את המקור של כל הזמנה
%% @end
%%%-------------------------------------------------------------------
-module(order_api).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([submit_order/1, submit_order/2,
         get_order_status/1, cancel_order/1,
         list_pending_orders/0, list_active_orders/0,
         get_api_stats/0, set_api_config/1]).

%% HTTP API handlers (for integration)
-export([handle_http_request/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    api_enabled = true :: boolean(),
    rate_limit = 100 :: integer(),  %% הזמנות לדקה
    rate_counter = #{} :: map(),    %% מונה לכל מקור
    sources_stats = #{} :: map(),   %% סטטיסטיקות לפי מקור
    auth_tokens = [] :: [binary()], %% טוקנים מורשים
    pending_orders = [] :: list()   %% הזמנות ממתינות
}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% הגשת הזמנה חדשה
submit_order(OrderData) ->
    submit_order(OrderData, internal).

%% הגשת הזמנה חדשה עם ציון המקור
submit_order(OrderData, Source) ->
    gen_server:call(?SERVER, {submit_order, OrderData, Source}).

%% קבלת סטטוס הזמנה
get_order_status(OrderID) ->
    gen_server:call(?SERVER, {get_status, OrderID}).

%% ביטול הזמנה
cancel_order(OrderID) ->
    gen_server:call(?SERVER, {cancel_order, OrderID}).

%% רשימת הזמנות ממתינות
list_pending_orders() ->
    gen_server:call(?SERVER, list_pending).

%% רשימת הזמנות פעילות
list_active_orders() ->
    gen_server:call(?SERVER, list_active).

%% קבלת סטטיסטיקות API
get_api_stats() ->
    gen_server:call(?SERVER, get_stats).

%% הגדרת קונפיגורציית API
set_api_config(Config) ->
    gen_server:call(?SERVER, {set_config, Config}).

%% טיפול בבקשת HTTP
handle_http_request(Method, Request) ->
    gen_server:call(?SERVER, {http_request, Method, Request}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    %% טעינת קונפיגורציית API
    {Enabled, RateLimit, AuthTokens} = load_api_config(),
    
    %% הפעלת טיימר לניקוי rate limit
    erlang:send_after(60000, self(), reset_rate_limits),
    
    io:format("Order API started. Enabled: ~p, Rate limit: ~p req/min~n", 
              [Enabled, RateLimit]),
    
    {ok, #state{
        api_enabled = Enabled,
        rate_limit = RateLimit,
        auth_tokens = AuthTokens,
        rate_counter = #{},
        sources_stats = #{},
        pending_orders = []
    }}.

handle_call({submit_order, OrderData, Source}, _From, State) ->
    %% בדיקה אם ה-API מופעל
    case State#state.api_enabled of
        false ->
            {reply, {error, api_disabled}, State};
        true ->
            %% בדיקת rate limit
            case check_rate_limit(Source, State) of
                {ok, NewState} ->
                    %% עיבוד ההזמנה
                    process_external_order(OrderData, Source, NewState);
                {error, rate_limit_exceeded} ->
                    {reply, {error, rate_limit_exceeded}, State}
            end
    end;

handle_call({get_status, OrderID}, _From, State) ->
    %% קבלת סטטוס מה-repository
    Status = case order_repository:get_order(OrderID) of
        {ok, Order} ->
            #{
                id => OrderID,
                status => maps:get(status, Order),
                assigned_courier => maps:get(assigned_courier, Order, undefined),
                created_at => maps:get(created_at, Order),
                updated_at => maps:get(last_update, Order, undefined)
            };
        {error, not_found} ->
            {error, order_not_found}
    end,
    {reply, Status, State};

handle_call({cancel_order, OrderID}, _From, State) ->
    %% ביטול הזמנה
    Result = cancel_order_internal(OrderID),
    {reply, Result, State};

handle_call(list_pending, _From, State) ->
    {reply, State#state.pending_orders, State};

handle_call(list_active, _From, State) ->
    ActiveOrders = order_repository:get_active_orders(),
    {reply, ActiveOrders, State};

handle_call(get_stats, _From, State) ->
    Stats = compile_api_statistics(State),
    {reply, Stats, State};

handle_call({set_config, Config}, _From, State) ->
    NewState = apply_api_config(Config, State),
    {reply, ok, NewState};

handle_call({http_request, Method, Request}, _From, State) ->
    %% טיפול בבקשת HTTP
    Response = handle_http_api(Method, Request, State),
    {reply, Response, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(reset_rate_limits, State) ->
    %% איפוס מוני rate limit
    erlang:send_after(60000, self(), reset_rate_limits),
    {noreply, State#state{rate_counter = #{}}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% פונקציות עיבוד פנימיות
%% ===================================================================

%% עיבוד הזמנה חיצונית
process_external_order(OrderData, Source, State) ->
    %% הוספת מטא-דאטה להזמנה
    EnrichedOrder = OrderData#{
        source => Source,
        received_at => erlang:system_time(millisecond),
        api_version => "1.0"
    },
    
    %% ולידציה של ההזמנה
    case order_validator:validate_order(EnrichedOrder) of
        {ok, ValidatedOrder} ->
            %% יצירת ID אם לא קיים
            FinalOrder = ensure_order_id(ValidatedOrder),
            OrderID = maps:get(id, FinalOrder),
            
            %% שמירה ב-repository
            order_repository:store_order(FinalOrder),
            
            %% שליחה לתור
            courier_manager:add_to_queue(FinalOrder),
            
            %% עדכון סטטיסטיקות
            NewStats = update_source_stats(Source, success, State#state.sources_stats),
            
            io:format("API: Order ~p received from ~p and queued.~n", [OrderID, Source]),
            
            {reply, {ok, OrderID}, State#state{sources_stats = NewStats}};
        
        {error, ValidationError} ->
            %% עדכון סטטיסטיקות כישלון
            NewStats = update_source_stats(Source, {failed, ValidationError}, 
                                          State#state.sources_stats),
            
            io:format("API: Order from ~p failed validation: ~p~n", 
                     [Source, ValidationError]),
            
            {reply, {error, ValidationError}, State#state{sources_stats = NewStats}}
    end.

%% בדיקת rate limit
check_rate_limit(Source, State) ->
    Current = maps:get(Source, State#state.rate_counter, 0),
    if
        Current >= State#state.rate_limit ->
            {error, rate_limit_exceeded};
        true ->
            NewCounter = maps:put(Source, Current + 1, State#state.rate_counter),
            {ok, State#state{rate_counter = NewCounter}}
    end.

%% וידוא שיש ID להזמנה
ensure_order_id(Order) ->
    case maps:is_key(id, Order) of
        true -> Order;
        false -> Order#{id => generate_order_id()}
    end.

%% יצירת ID ייחודי להזמנה
generate_order_id() ->
    %% שימוש ב-timestamp + random לייחודיות
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(9999),
    list_to_atom(lists:flatten(io_lib:format("api_order_~p_~p", [Timestamp, Random]))).

%% ביטול הזמנה
cancel_order_internal(OrderID) ->
    case order_repository:get_order(OrderID) of
        {ok, Order} ->
            Status = maps:get(status, Order),
            case Status of
                pending ->
                    %% אפשר לבטל - עדכון סטטוס
                    order_repository:update_order_status(OrderID, cancelled),
                    {ok, cancelled};
                assigned ->
                    %% צריך לשחרר את השליח
                    CourierID = maps:get(assigned_courier, Order),
                    courier_manager:release_courier(CourierID),
                    order_repository:update_order_status(OrderID, cancelled),
                    {ok, cancelled};
                completed ->
                    {error, order_already_completed};
                failed ->
                    {error, order_already_failed};
                cancelled ->
                    {error, order_already_cancelled}
            end;
        {error, not_found} ->
            {error, order_not_found}
    end.

%% עדכון סטטיסטיקות מקור
update_source_stats(Source, Result, StatsMap) ->
    SourceStats = maps:get(Source, StatsMap, #{
        total => 0,
        success => 0,
        failed => 0,
        errors => []
    }),
    
    UpdatedStats = case Result of
        success ->
            SourceStats#{
                total => maps:get(total, SourceStats) + 1,
                success => maps:get(success, SourceStats) + 1
            };
        {failed, Error} ->
            Errors = maps:get(errors, SourceStats),
            SourceStats#{
                total => maps:get(total, SourceStats) + 1,
                failed => maps:get(failed, SourceStats) + 1,
                errors => [{erlang:system_time(millisecond), Error} | 
                          lists:sublist(Errors, 10)] % שמירת 10 שגיאות אחרונות
            }
    end,
    
    maps:put(Source, UpdatedStats, StatsMap).

%% קומפילציה של סטטיסטיקות API
compile_api_statistics(State) ->
    TotalRequests = lists:sum([maps:get(total, Stats, 0) || 
                               {_, Stats} <- maps:to_list(State#state.sources_stats)]),
    TotalSuccess = lists:sum([maps:get(success, Stats, 0) || 
                              {_, Stats} <- maps:to_list(State#state.sources_stats)]),
    
    #{
        enabled => State#state.api_enabled,
        rate_limit => State#state.rate_limit,
        current_rate => maps:to_list(State#state.rate_counter),
        total_requests => TotalRequests,
        total_success => TotalSuccess,
        success_rate => case TotalRequests of
            0 -> 100.0;
            _ -> (TotalSuccess / TotalRequests) * 100
        end,
        sources => State#state.sources_stats
    }.

%% החלת קונפיגורציית API
apply_api_config(Config, State) ->
    State#state{
        api_enabled = maps:get(enabled, Config, State#state.api_enabled),
        rate_limit = maps:get(rate_limit, Config, State#state.rate_limit),
        auth_tokens = maps:get(auth_tokens, Config, State#state.auth_tokens)
    }.

%% טיפול ב-HTTP API
handle_http_api(post, #{path := "/orders", body := Body, headers := Headers}, State) ->
    %% בדיקת אימות
    case authenticate_request(Headers, State#state.auth_tokens) of
        ok ->
            %% פרסור ה-body (JSON למשל)
            OrderData = parse_request_body(Body),
            Source = get_source_from_headers(Headers),
            
            %% הגשת ההזמנה
            case process_external_order(OrderData, Source, State) of
                {reply, {ok, OrderID}, NewState} ->
                    {{200, #{order_id => OrderID}}, NewState};
                {reply, {error, Reason}, NewState} ->
                    {{400, #{error => Reason}}, NewState}
            end;
        {error, unauthorized} ->
            {{401, #{error => unauthorized}}, State}
    end;

handle_http_api(get, #{path := "/orders/" ++ OrderID}, _State) ->
    %% קבלת סטטוס הזמנה
    case order_repository:get_order(list_to_atom(OrderID)) of
        {ok, Order} ->
            {200, format_order_for_api(Order)};
        {error, not_found} ->
            {404, #{error => order_not_found}}
    end;

handle_http_api(delete, #{path := "/orders/" ++ OrderID}, _State) ->
    %% ביטול הזמנה
    case cancel_order_internal(list_to_atom(OrderID)) of
        {ok, cancelled} ->
            {200, #{status => cancelled}};
        {error, Reason} ->
            {400, #{error => Reason}}
    end;

handle_http_api(get, #{path := "/stats"}, State) ->
    Stats = compile_api_statistics(State),
    {200, Stats};

handle_http_api(_, _, _State) ->
    {404, #{error => endpoint_not_found}}.

%% אימות בקשה
authenticate_request(Headers, AuthTokens) ->
    case maps:get(<<"authorization">>, Headers, undefined) of
        undefined ->
            {error, unauthorized};
        <<"Bearer ", Token/binary>> ->
            case lists:member(Token, AuthTokens) of
                true -> ok;
                false -> {error, unauthorized}
            end;
        _ ->
            {error, unauthorized}
    end.

%% פרסור body של בקשה
parse_request_body(Body) when is_binary(Body) ->
    %% כאן צריך לפרסר JSON או פורמט אחר
    %% לדוגמה בסיסית:
    #{
        business_location => {50, 50},
        customer_location => {75, 75},
        items => []
    };
parse_request_body(Body) when is_map(Body) ->
    Body.

%% קבלת מקור מה-headers
get_source_from_headers(Headers) ->
    case maps:get(<<"x-api-client">>, Headers, undefined) of
        undefined -> http_api;
        Client -> binary_to_atom(Client, utf8)
    end.

%% פורמט הזמנה ל-API
format_order_for_api(Order) ->
    #{
        id => maps:get(id, Order),
        status => maps:get(status, Order),
        created_at => maps:get(created_at, Order),
        business_location => maps:get(business_location, Order),
        customer_location => maps:get(customer_location, Order),
        assigned_courier => maps:get(assigned_courier, Order, null),
        source => maps:get(source, Order, internal)
    }.

%% טעינת קונפיגורציית API
load_api_config() ->
    %% טעינה מקונפיגורציה או ברירת מחדל
    Enabled = application:get_env(delivery_system_orders, api_enabled, true),
    RateLimit = application:get_env(delivery_system_orders, api_rate_limit, 100),
    AuthTokens = application:get_env(delivery_system_orders, api_auth_tokens, []),
    {Enabled, RateLimit, AuthTokens}.