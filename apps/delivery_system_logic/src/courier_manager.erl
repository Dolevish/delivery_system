%%%-------------------------------------------------------------------
%% @doc courier manager responsible for managing courier assignments.
%%      It handles the assignment and release of couriers for deliveries.
%%      manage two ETS tables: one for available couriers and one for busy couriers and a queue for waiting orders.
%%      FIXED: Added process monitoring to handle courier process failures
%%      עודכן: עובד עם order_repository במקום לנהל בעצמו נתוני הזמנות
%% @end
%%%-------------------------------------------------------------------
-module(courier_manager).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([add_to_queue/1, release_courier/1, get_queue_status/0, get_courier_status/0]). 


%% gen_server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    available_tid, 
    busy_tid, 
    waiting_orders :: queue:queue(),
    monitors = #{} :: #{reference() => atom()},  % MonitorRef -> CourierID
    %% סטטיסטיקות נוספות
    total_assignments = 0 :: integer(),
    total_completions = 0 :: integer(),
    total_failures = 0 :: integer()
}).


%% ===================================================================
%% Implementation of the API
%% ===================================================================

%% Initializes the gen_server with a list of initial couriers.
start_link(InitialCouriers) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitialCouriers, []).


%% Releases a courier after completing a task.
%%      This is an asynchronous (cast) - the calling process does not wait for a response.
release_courier(CourierID) ->
    gen_server:cast(?SERVER, {release_courier, CourierID}).


%% Adds a new order to the orders queue.
add_to_queue(OrderData) ->
    gen_server:cast(?SERVER, {add_to_queue, OrderData}).

%% קבלת סטטוס התור
get_queue_status() ->
    gen_server:call(?SERVER, get_queue_status).

%% קבלת סטטוס השליחים
get_courier_status() ->
    gen_server:call(?SERVER, get_courier_status).


%% ===================================================================
%% gen_server Callbacks
%% ===================================================================

%% Initializes the gen_server state.
init(InitialCouriers) ->
    
    %% Create ETS tables to manage couriers.
    AvailableTID = ets:new(available_couriers, [set, named_table, public]),
    BusyTID = ets:new(busy_couriers, [set, named_table, public]),

    %% Add initial couriers to the available table - each with a unique key.
    lists:foreach(fun(CourierData) ->
        CourierID = maps:get(id, CourierData),
        ets:insert(AvailableTID, {CourierID, CourierData})  % Unique key.
    end, InitialCouriers),

    io:format("Courier manager started with ~p couriers.~n", [length(InitialCouriers)]),

    %% Initialize the waiting orders queue.
    WaitingOrdersQueue = queue:new(),

    %% The state now includes an empty monitors map
    {ok, #state{
        available_tid = AvailableTID, 
        busy_tid = BusyTID, 
        waiting_orders = WaitingOrdersQueue,
        monitors = #{}
    }}.



%% Handles synchronous calls (gen_server:call).
handle_call(get_queue_status, _From, State) ->
    QueueLength = queue:len(State#state.waiting_orders),
    Status = #{
        queue_length => QueueLength,
        orders_waiting => queue:to_list(State#state.waiting_orders)
    },
    {reply, Status, State};

handle_call(get_courier_status, _From, State) ->
    NumAvailable = ets:info(State#state.available_tid, size),
    NumBusy = ets:info(State#state.busy_tid, size),
    
    Status = #{
        available => NumAvailable,
        busy => NumBusy,
        total => NumAvailable + NumBusy,
        busy_details => ets:tab2list(State#state.busy_tid),
        statistics => #{
            total_assignments => State#state.total_assignments,
            total_completions => State#state.total_completions,
            total_failures => State#state.total_failures
        }
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.



%% Handles asynchronous casts (gen_server:cast):

%% Adding a new order to the waiting queue.
handle_cast({add_to_queue, OrderData}, State = #state{waiting_orders = Q}) ->
    OrderID = maps:get(id, OrderData),
    io:format("Courier manager: Adding order ~p to the waiting queue.~n", [OrderID]),
    
    %% עדכון סטטוס ההזמנה ב-repository
    order_repository:update_order_status(OrderID, queued),
    
    %% Adding the new order to the end of the queue.
    NewQ = queue:in(OrderData, Q),
    NewState = State#state{waiting_orders = NewQ},

    %% Immediately after adding, try to dispatch a courier.
    try_dispatch(NewState);


%% Releases a courier and makes it available for new tasks.
handle_cast({release_courier, CourierID}, State = #state{available_tid = AvailTID, busy_tid = BusyTID, monitors = Monitors}) ->
    io:format("Courier manager: Courier ~p has been released.~n", [CourierID]),

    %% Cancel the monitor if it exists
    NewMonitors = case find_monitor_by_courier(CourierID, Monitors) of
        {ok, MonRef} ->
            %% Cancel the monitor since the courier finished successfully
            erlang:demonitor(MonRef, [flush]),
            maps:remove(MonRef, Monitors);
        not_found ->
            Monitors
    end,

    %% Searching for the courier in the busy table
    case ets:lookup(BusyTID, CourierID) of
        [{CourierID, CourierData, OrderID}] ->  
            %% Found the courier - removing from the busy table
            ets:delete(BusyTID, CourierID),

            %% עדכון ההזמנה כהושלמה ב-repository
            order_repository:mark_order_completed(OrderID),
            
            %% עדכון סטטיסטיקות
            NewCompletions = State#state.total_completions + 1,

            %% Adding back to the available table
            ets:insert(AvailTID, {CourierID, CourierData}),

            io:format("Courier manager: Courier ~p is now available for new orders.~n", [CourierID]),

            %% Update state with new monitors map and statistics
            NewState = State#state{
                monitors = NewMonitors,
                total_completions = NewCompletions
            },

            %% Trying to dispatch the courier for a new task from the queue
            try_dispatch(NewState);
        [] ->
            %% Could not find the courier
            io:format("*******************************************************************~n"),
            io:format("Warning: Could not find courier ~p in busy table to release.~n", [CourierID]),
            io:format("*******************************************************************~n"),
            NewState = State#state{monitors = NewMonitors},
            try_dispatch(NewState)
    end;

%% טיפול בעדכון קונפיגורציה
handle_cast({config_update, max_orders, MaxOrders}, State) ->
    io:format("Courier manager: Max concurrent orders updated to ~p~n", [MaxOrders]),
    %% ניתן להוסיף לוגיקה נוספת כאן אם נדרש
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%% DOWN message handler - monitor process termination
handle_info({'DOWN', MonRef, process, _Pid, Reason}, 
            State = #state{available_tid = AvailTID, busy_tid = BusyTID, monitors = Monitors}) ->

    %% Find out which courier it was
    case maps:get(MonRef, Monitors, undefined) of
        undefined ->
            %% Monitor not found - ignore
            {noreply, State};
        CourierID ->
            %% Courier crashed! Need to handle it
            io:format("WARNING: Courier ~p process died with reason: ~p~n", [CourierID, Reason]),

            %% עדכון סטטיסטיקות
            NewFailures = State#state.total_failures + 1,

            %% Find the courier in the busy table
            case ets:lookup(BusyTID, CourierID) of
                [{CourierID, CourierData, OrderID}] ->
                    %% Remove from busy
                    ets:delete(BusyTID, CourierID),

                    %% Return to available (the courier will be restarted by the supervisor)
                    ets:insert(AvailTID, {CourierID, CourierData}),
                    
                    io:format("Courier ~p recovered and marked as available. Order ~p will be reassigned.~n", 
                              [CourierID, OrderID]),
                    
                    %% קבלת נתוני ההזמנה מה-repository
                    case order_repository:get_order(OrderID) of
                        {ok, OrderData} ->
                            %% עדכון סטטוס ההזמנה לכשלון ב-repository
                            order_repository:mark_order_failed(OrderID, courier_crashed),
                            
                            %% Return the order to the front of the queue (high priority)
                            NewQ = queue:in_r(OrderData, State#state.waiting_orders), %%Insert at the front
                            
                            io:format("Order ~p returned to queue for reassignment.~n", [OrderID]),
                            
                            NewStateWithOrder = State#state{
                                waiting_orders = NewQ,
                                total_failures = NewFailures
                            },
                            try_dispatch(NewStateWithOrder);
                        {error, not_found} ->
                            %% No information about the order - just log and continue
                            io:format("Warning: Could not recover order ~p data.~n", [OrderID]),
                            {noreply, State#state{total_failures = NewFailures}}
                    end;
                [] ->
                    %% The courier was not in the busy table, probably already released
                    {noreply, State#state{total_failures = NewFailures}}
            end,

            %% Remove the monitor from the map
            NewMonitors = maps:remove(MonRef, Monitors),
            {noreply, State#state{monitors = NewMonitors}}
    end;

%% טיפול בעדכוני קונפיגורציה מהמחולל
handle_info({config_update, Type, Value}, State) ->
    io:format("Courier manager: Received config update - ~p: ~p~n", [Type, Value]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% דיווח סטטיסטיקות סופיות
    io:format("Courier manager terminating. Statistics:~n"),
    io:format("  Total assignments: ~p~n", [State#state.total_assignments]),
    io:format("  Total completions: ~p~n", [State#state.total_completions]),
    io:format("  Total failures: ~p~n", [State#state.total_failures]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

%% Find monitor by CourierID
find_monitor_by_courier(CourierID, Monitors) ->
    case lists:keyfind(CourierID, 2, maps:to_list(Monitors)) of
        {MonRef, CourierID} -> {ok, MonRef};
        false -> not_found
    end.

%% Attempts to dispatch available couriers to waiting orders.
try_dispatch(State = #state{waiting_orders = Q, monitors = Monitors}) ->
    %% Check if there are any waiting orders
    case queue:is_empty(Q) of
        true ->
            %% No waiting orders
            NumAvailable = ets:info(available_couriers, size),
            if 
                NumAvailable > 0 ->
                    io:format("Courier manager: ~p couriers available, no orders in queue~n", [NumAvailable]);
                true -> ok
            end,
            {noreply, State};
        false ->
            %% There are waiting orders - looking for an available courier
            case ets:tab2list(available_couriers) of
                [] ->
                    %% No available couriers
                    QueueLength = queue:len(Q),
                    io:format("Courier manager: No available couriers, orders waiting in queue: ~p~n", [QueueLength]),
                    {noreply, State};
                [{CourierID, CourierData} | _] -> 
                    %% There is at least one available courier - take the first one

                    %% Remove the first order from the queue (FIFO)
                    {{value, OrderData}, NewQ} = queue:out(Q),

                    OrderID = maps:get(id, OrderData),

                    %% Move the courier from available to busy
                    ets:delete(available_couriers, CourierID),
                    ets:insert(busy_couriers, {CourierID, CourierData, OrderID}),

                    io:format("Courier manager: Dispatching courier ~p for order ~p.~n", [CourierID, OrderID]),

                    %% עדכון סטטוס ההזמנה ב-repository
                    order_repository:mark_order_assigned(OrderID, CourierID),

                    %% Create job data for the courier
                    JobData = #{
                        order_id => OrderID,
                        business_location => maps:get(business_location, OrderData),
                        customer_location => maps:get(customer_location, OrderData),
                        priority => maps:get(priority, OrderData, normal)
                    },

                    %% Send job data to the courier process
                    case whereis(CourierID) of
                        undefined ->
                            io:format("Error: Courier process ~p not found!~n", [CourierID]),
                            %% Return the courier to available and the order to the queue
                            ets:insert(available_couriers, {CourierID, CourierData}),
                            ets:delete(busy_couriers, CourierID),
                            
                            %% עדכון סטטוס ההזמנה חזרה ל-queued
                            order_repository:update_order_status(OrderID, queued),
                            
                            NewStateWithOrder = State#state{waiting_orders = queue:in_r(OrderData, NewQ)},
                            {noreply, NewStateWithOrder};
                        CourierPid ->
                            %% Create a monitor for the courier process
                            MonRef = erlang:monitor(process, CourierPid),
                            NewMonitors = maps:put(MonRef, CourierID, Monitors),
                            
                            %% Send message to the courier process
                            gen_statem:cast(CourierPid, {assign_job, JobData}),

                            %% עדכון סטטיסטיקות
                            NewAssignments = State#state.total_assignments + 1,

                            %% Update the state with the new queue and monitors
                            NewState = State#state{
                                waiting_orders = NewQ,
                                monitors = NewMonitors,
                                total_assignments = NewAssignments
                            },

                            %% Call the function again to check for more couriers and orders
                            try_dispatch(NewState)
                    end
            end
    end.