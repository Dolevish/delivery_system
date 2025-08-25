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

    %% Insert initial couriers into the available couriers table.
    [ets:insert(AvailableTID, {courier, CourierData}) || CourierData <- InitialCouriers],

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

    %% Look for the courier in the busy table.
    case ets:match_object(BusyTID, {courier, #{id => CourierID}, '_'}) of
        [FullBusyRecord | _] ->
            %% If found, remove from busy table.
            ets:delete_object(BusyTID, FullBusyRecord),

            %% Extract the courier data from the record.
            {courier, CourierData, _OrderID} = FullBusyRecord,

            %% Insert back into the available table.
            ets:insert(AvailTID, {courier, CourierData}),

            %% Immediately after adding, we try to dispatch it to a new task.
            try_dispatch(State);
        [] ->
            %% Edge case: We didn't find such a courier. This shouldn't happen.
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
    %% Check if there are both available couriers and waiting orders.
    case {ets:first(available_couriers), queue:is_empty(Q)} of
        {'$end_of_table', _} ->
            %% No available couriers, nothing to do.
            {noreply, State};

        {_, true} ->
            %% There are available couriers but no waiting orders, nothing to do.
            {noreply, State};

        {CourierKey, false} ->
            %% There is both an available courier and a waiting order

            %% Remove the first order from the queue.
            {{value, OrderData}, NewQ} = queue:out(Q),

            %% Retrieve the courier details.
            [{courier, CourierData}] = ets:lookup(available_couriers, CourierKey),

            %% Reserve the courier.
            ets:delete(available_couriers, CourierKey),
            ets:insert(busy_couriers, {courier, CourierData, maps:get(id, OrderData)}),

            io:format("Courier manager: Dispatching courier ~p for order ~p.~n", [maps:get(id, CourierData), maps:get(id, OrderData)]),

            %% Start the order process:

            %% Merge order and courier data
            CourierID = maps:get(id, CourierData),
            CombinedData = maps:merge(OrderData, #{courier_id => CourierID}),

            %% Order process supervisor starts a new process.
            {ok, _Pid} = delivery_system_orders_sup:start_order_process(CombinedData),

            %% Update the internal state of the dispatcher with the new queue.
            NewState = State#state{waiting_orders = NewQ},

            %% We call the function again recursively to see if we can dispatch more couriers to additional orders.
            try_dispatch(NewState)
    end.