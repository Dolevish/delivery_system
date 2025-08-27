%%%-------------------------------------------------------------------
%% @doc courier manager responsible for managing courier assignments.
%%      It handles the assignment and release of couriers for deliveries.
%%      manage two ETS tables: one for available couriers and one for busy couriers and a queue for waiting orders.
%% @end
%%%-------------------------------------------------------------------
-module(courier_manager).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([add_to_queue/1, release_courier/1]). 


%% gen_server Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {available_tid, busy_tid, waiting_orders :: queue:queue()}).


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

    %% The state of the gen_server is a record containing the table IDs.
    {ok, #state{available_tid = AvailableTID, busy_tid = BusyTID, waiting_orders = WaitingOrdersQueue}}.



%% Handles synchronous calls (gen_server:call).
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.



%% Handles asynchronous casts (gen_server:cast):

%% Adding a new order to the waiting queue.
handle_cast({add_to_queue, OrderData}, State = #state{waiting_orders = Q}) ->
    io:format("Courier manager: Adding order ~p to the waiting queue.~n", [maps:get(id, OrderData)]),
    
    %% Adding the new order to the end of the queue.
    NewQ = queue:in(OrderData, Q),
    NewState = State#state{waiting_orders = NewQ},

    %% Immediately after adding, try to dispatch a courier.
    try_dispatch(NewState);

%% Releases a courier and makes it available for new tasks.
handle_cast({release_courier, CourierID}, State = #state{available_tid = AvailTID, busy_tid = BusyTID}) ->
    io:format("Courier manager: Courier ~p has been released.~n", [CourierID]),

    %% Searching for the courier in the busy table
    case ets:lookup(BusyTID, CourierID) of
        [{CourierID, CourierData, _OrderID}] ->  
            %% Found the courier - removing from the busy table
            ets:delete(BusyTID, CourierID),

            %% Adding back to the available table
            ets:insert(AvailTID, {CourierID, CourierData}),

            io:format("Courier manager: Courier ~p is now available for new orders.~n", [CourierID]),

            %% Trying to dispatch the courier for a new task from the queue
            try_dispatch(State);
        [] ->
            %% Could not find the courier
            io:format("Warning: Could not find courier ~p in busy table to release.~n", [CourierID]),
            try_dispatch(State)
    end;
    

handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%% ===================================================================
%% Internal functions
%% ===================================================================

%% Attempts to dispatch available couriers to waiting orders.
try_dispatch(State = #state{waiting_orders = Q}) ->
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
                    %% FIX: השתמש באותו מזהה עקבי - CourierID הוא המפתח בטבלה
                    %% There is at least one available courier - take the first one

                    %% Remove the first order from the queue (FIFO)
                    {{value, OrderData}, NewQ} = queue:out(Q),

                    %% Move the courier from available to busy
                    ets:delete(available_couriers, CourierID),  % Delete by key
                    ets:insert(busy_couriers, {CourierID, CourierData, maps:get(id, OrderData)}),

                    %% FIX: השתמש ב-CourierID שהוא כבר המפתח, לא צריך לחלץ אותו שוב
                    OrderID = maps:get(id, OrderData),
                    
                    io:format("Courier manager: Dispatching courier ~p for order ~p.~n", [CourierID, OrderID]),

                    %% Create job data for the courier
                    JobData = #{
                        order_id => OrderID,
                        business_location => maps:get(business_location, OrderData),
                        customer_location => maps:get(customer_location, OrderData)
                    },

                    %% Send job data to the courier process
                    case whereis(CourierID) of
                        undefined ->
                            io:format("Error: Courier process ~p not found!~n", [CourierID]),
                            %% Return the courier to available and the order to the queue
                            ets:insert(available_couriers, {CourierID, CourierData}),
                            %% FIX: תיקון המחיקה - מוחקים רק עם המפתח
                            ets:delete(busy_couriers, CourierID),
                            NewStateWithOrder = State#state{waiting_orders = queue:in_r(OrderData, NewQ)},
                            {noreply, NewStateWithOrder};
                        CourierPid ->
                            %% Send message to the courier process
                            gen_statem:cast(CourierPid, {assign_job, JobData}),

                            %% Update the state with the new queue
                            NewState = State#state{waiting_orders = NewQ},

                            %% Call the function again to check for more couriers and orders
                            try_dispatch(NewState)
                    end
            end
    end.