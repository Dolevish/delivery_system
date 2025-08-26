%%%-------------------------------------------------------------------
%% @doc courier process representing a single courier, implemented as a state machine.
%%      Responsible for simulating its movement and managing its state (available, delivering, etc.).
%%%-------------------------------------------------------------------
-module(courier_process).
-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem Callbacks
-export([init/1, callback_mode/0, terminate/3]).

%% For each state, export its handling function
-export([available/3, driving_to_business/3, driving_to_customer/3]).




%% Starts the courier process.
%% The gen_statem will be registered under the unique identifier of the courier.
start_link(CourierID) ->
    gen_statem:start_link({local, CourierID}, ?MODULE, [CourierID], []).


%% Defines the callback_mode.
%% state_functions means that each state will have its own function.
callback_mode() ->
    state_functions.


%% Initializes the state machine with the given CourierID.
init([CourierID]) ->
    io:format("Courier statem ~p started.~n", [CourierID]),
    %% The initial state is 'available'.
    %% The state's Data will contain information about the courier.
    {ok, available, #{id => CourierID, location => {0,0}}}.




%% ===================================================================
%% State handling functions
%% ===================================================================


%% Handles events when the courier is in the 'available' state.
available(cast, {assign_job, JobData}, Data) ->
    %% Extract relevant information from JobData.
    CourierID = maps:get(id, Data),
    OrderID = maps:get(order_id, JobData),
    BusinessLocation = maps:get(business_location, JobData),
    CustomerLocation = maps:get(customer_location, JobData),
    CurrentLocation = maps:get(location, Data),
    
    io:format("Courier ~p assigned to order ~p, driving to business at ~p~n", 
              [CourierID, OrderID, BusinessLocation]),

    %% Calculate travel time to the business
    TravelTime = calculate_travel_time(CurrentLocation, BusinessLocation),

    %% Start driving to business - send ourselves a message when we arrive
    erlang:send_after(TravelTime, self(), arrived_at_business),

    %% Transition to the next state: driving to business.
    %% Update the internal Data with the job details.
    NewData = Data#{
        order_id => OrderID, 
        business_location => BusinessLocation, 
        customer_location => CustomerLocation
    },
    
    {next_state, driving_to_business, NewData};

available(EventType, EventContent, Data) ->
    %% Handle unexpected events in this state.
    io:format("Courier ~p: Unhandled event in 'available' state: ~p, ~p~n", 
              [maps:get(id, Data), EventType, EventContent]),
    {keep_state, Data}.




%% Handles events when the courier is en route to the business.
driving_to_business(info, arrived_at_business, Data) ->
    CourierID = maps:get(id, Data),
    OrderID = maps:get(order_id, Data),
    BusinessLocation = maps:get(business_location, Data),
    CustomerLocation = maps:get(customer_location, Data),
    
    io:format("Courier ~p arrived at business for order ~p, now driving to customer at ~p~n", 
              [CourierID, OrderID, CustomerLocation]),
    
    %% Update current location to business
    UpdatedData = maps:put(location, BusinessLocation, Data),

    %% Calculate travel time to customer
    TravelTime = calculate_travel_time(BusinessLocation, CustomerLocation),

    %% Start driving to customer - send ourselves a message when we arrive
    erlang:send_after(TravelTime, self(), arrived_at_customer),
    
    %% Transition to the next state: driving to customer.
    {next_state, driving_to_customer, UpdatedData};

driving_to_business(EventType, EventContent, Data) ->
    %% Handle unexpected events in this state.
    io:format("Courier ~p: Unhandled event in 'driving_to_business' state: ~p, ~p~n", 
              [maps:get(id, Data), EventType, EventContent]),
    {keep_state, Data}.




%% Handles events when the courier is en route to the customer.
driving_to_customer(info, arrived_at_customer, Data) ->
    CourierID = maps:get(id, Data),
    OrderID = maps:get(order_id, Data),
    CustomerLocation = maps:get(customer_location, Data),
    
    io:format("Courier ~p delivered order ~p successfully!~n", [CourierID, OrderID]),

    %% Update current location to customer
    UpdatedData = maps:put(location, CustomerLocation, Data),

    %% Notify the courier manager that the courier is available
    courier_manager:release_courier(CourierID),

    %% Return to available state and clear order details
    CleanData = maps:remove(order_id, 
                 maps:remove(business_location, 
                  maps:remove(customer_location, UpdatedData))),
    
    %% Transition back to the initial state, ready for the next task.
    {next_state, available, CleanData};

driving_to_customer(EventType, EventContent, Data) ->
    %% Handle unexpected events in this state.
    io:format("Courier ~p: Unhandled event in 'driving_to_customer' state: ~p, ~p~n", 
              [maps:get(id, Data), EventType, EventContent]),
    {keep_state, Data}.




%% Called when the process terminates.
terminate(_Reason, _State, _Data) ->
    ok.


%% ===================================================================
%% Helper functions
%% ===================================================================

%% Calculate distance between two points
calculate_distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

%% Calculate travel time (in milliseconds) - more distance = more time
%% Each distance unit = 200 milliseconds + random up to 100 milliseconds
calculate_travel_time(From, To) ->
    Distance = calculate_distance(From, To),
    BaseTime = round(Distance * 200), % 200ms per distance unit
    RandomDelay = rand:uniform(100), % up to 100ms random
    MaxTime = max(BaseTime + RandomDelay, 1000), % minimum 1 second
    io:format("Travel time from ~p to ~p: ~p ms (distance: ~.2f)~n", 
              [From, To, MaxTime, Distance]),
    MaxTime.