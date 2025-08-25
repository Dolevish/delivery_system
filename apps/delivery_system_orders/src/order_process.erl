%%%-------------------------------------------------------------------
%% order_process - state machine managing the lifecycle of a single order.
%%%-------------------------------------------------------------------
-module(order_process).
-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem Callbacks
-export([init/1, callback_mode/0, terminate/3]).

%% State functions
-export([awaiting_pickup/3, in_delivery/3]).


start_link(OrderAndCourierData) ->
    gen_statem:start_link(?MODULE, [OrderAndCourierData], []).

%% State callback mode.
callback_mode() ->
    state_functions.

%% Init function to set the initial state and data.
init([OrderAndCourierData]) ->
    OrderID = maps:get(id, OrderAndCourierData),
    CourierID = maps:get(courier_id, OrderAndCourierData),
    io:format("Order process ~p started, assigned to courier ~p.~n", [OrderID, CourierID]),
    
    %% Initial state is "awaiting_pickup".
    {ok, awaiting_pickup, OrderAndCourierData, {state_timeout, 3000, courier_arrived_at_pickup}}.

%% ===================================================================
%% State handling functions
%% ===================================================================

%% Handles events when the order is awaiting pickup from the store.
awaiting_pickup(state_timeout, courier_arrived_at_pickup, Data) ->
    OrderID = maps:get(id, Data),
    io:format("Order ~p: Courier has picked up the package.~n", [OrderID]),

    %% Move to the next state: "in_delivery".
    %% Initiate timer for delivery process.
    {next_state, in_delivery, Data, {state_timeout, 5000, delivery_completed}}; % 5 seconds for now

awaiting_pickup(EventType, EventContent, Data) ->
    io:format("Order ~p: Unhandled event in 'awaiting_pickup' state: ~p, ~p~n", [maps:get(id, Data), EventType, EventContent]),
    {keep_state, Data}.



%% Handles events when the order is in delivery to the customer.
in_delivery(state_timeout, delivery_completed, Data) ->
    OrderID = maps:get(id, Data),
    CourierID = maps:get(courier_id, Data),
    io:format("Order ~p: Delivery completed by courier ~p.~n", [OrderID, CourierID]),
    
    %% Notify the courier manager that the courier is now available.
    courier_manager:release_courier(CourierID),

    %% TODO: Send order details to archive in db_worker.

    %% Finished the lifecycle of the process, stopping it cleanly.
    {stop, normal, Data};

in_delivery(EventType, EventContent, Data) ->
    io:format("Order ~p: Unhandled event in 'in_delivery' state: ~p, ~p~n", [maps:get(id, Data), EventType, EventContent]),
    {keep_state, Data}.



terminate(_Reason, _State, _Data) ->
    ok.