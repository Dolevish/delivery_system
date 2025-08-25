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
available({call, From}, {assign_job, JobData}, Data) ->
    %% Extract relevant information from JobData.
    CourierID = maps:get(id, Data),
    OrderID = maps:get(order_id, JobData),
    BusinessLocation = maps:get(business_location, JobData),
    CustomerLocation = maps:get(customer_location, JobData),
    
    io:format("Courier ~p assigned to order ~p~n", [CourierID, OrderID]),

    gen_statem:reply(From, ok),

    %% Transition to the next state: driving to business.
    %% Update the internal Data with the job details.
    NewData = Data#{
            order_id => OrderID, 
            destination => BusinessLocation, 
            final_destination => CustomerLocation
    },

    %% כאן נוסיף בעתיד את הלוגיקה שמתחילה את תנועת הסימולציה
    
    {next_state, driving_to_business, NewData};

available(EventType, EventContent, Data) ->
    %% Handle unexpected events in this state.
    io:format("Unhandled event in 'available' state: ~p, ~p~n", [EventType, EventContent]),
    {keep_state, Data}.




%% Handles events when the courier is en route to the business.
driving_to_business(info, arrived_at_business, Data) ->
    io:format("Courier ~p arrived at business for order ~p~n", [maps:get(id, Data), maps:get(order_id, Data)]),
    %% Transition to the next state: driving to customer.
    NewData = maps:put(dest, maps:get(final_dest, Data), Data),
    {next_state, driving_to_customer, NewData};

driving_to_business(EventType, EventContent, Data) ->
    %% Handle unexpected events in this state.
    io:format("Unhandled event in 'driving_to_business' state: ~p, ~p~n", [EventType, EventContent]),
    {keep_state, Data}.




%% Handles events when the courier is en route to the customer.
driving_to_customer(info, arrived_at_customer, Data) ->
    io:format("Courier ~p delivered order ~p~n", [maps:get(id, Data), maps:get(order_id, Data)]),
    %% Here, the process will notify the courier_manager that it has finished.
    courier_manager:release_courier(maps:get(id, Data)),

    %% Transition back to the initial state, ready for the next task.
    {next_state, available, maps:remove(order_id, Data)};

driving_to_customer(EventType, EventContent, Data) ->
    %% Handle unexpected events in this state.
    io:format("Unhandled event in 'driving_to_customer' state: ~p, ~p~n", [EventType, EventContent]),
    {keep_state, Data}.




%% Called when the process terminates.
terminate(_Reason, _State, _Data) ->
    ok.