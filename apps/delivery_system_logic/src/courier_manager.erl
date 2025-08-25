%%%-------------------------------------------------------------------
%% @doc courier manager responsible for managing courier assignments.
%%      It handles the assignment and release of couriers for deliveries.
%%      manage two ETS tables: one for available couriers and one for busy couriers.
%% @end
%%%-------------------------------------------------------------------
-module(courier_manager).

-behaviour(gen_server).

-export([start_link/1]).
-export([assign_courier/2, release_courier/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {available_tid, busy_tid}).


%% ===================================================================
%% Implementation of the API
%% ===================================================================

%% Initializes the gen_server with a list of initial couriers.
start_link(InitialCouriers) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitialCouriers, []).



%% Requests the assignment of a courier for an order.
%%     This is a synchronous (call) - the calling process will wait for a response.
assign_courier(OrderID, PickupLocation) ->
    gen_server:call(?SERVER, {assign_courier, OrderID, PickupLocation}).



%% Releases a courier after completing a task.
%%      This is an asynchronous (cast) - the calling process does not wait for a response.
release_courier(CourierID) ->
    gen_server:cast(?SERVER, {release_courier, CourierID}).





%% ===================================================================
%% Implementation of the gen_server Callbacks
%% ===================================================================

%% Initializes the gen_server state.
init(InitialCouriers) ->
    
    %% Create ETS tables to manage couriers.
    AvailableTID = ets:new(available_couriers, [set, named_table, public]),
    BusyTID = ets:new(busy_couriers, [set, named_table, public]),


    %% Insert initial couriers into the available couriers table.
    [ets:insert(AvailableTID, {courier, CourierData}) || CourierData <- InitialCouriers],

    io:format("Courier manager started with ~p couriers.~n", [length(InitialCouriers)]),


    %% The state of the gen_server is a record containing the table IDs.
    {ok, #state{available_tid = AvailableTID, busy_tid = BusyTID}}.



%% Handles synchronous calls (gen_server:call).
handle_call({assign_courier, OrderID, PickupLocation}, _From, State) ->

    %% TODO: בשלב הבא נשכלל את הלוגיקה למצוא את השליח הקרוב ביותר.
    %% כרגע, אנחנו לוקחים את השליח הראשון שאנחנו מוצאים.

case ets:first(available_couriers) of
        '$end_of_table' ->
            %% Empty table, no available couriers
            Reply = {error, no_available_couriers},
            {reply, Reply, State};
            
        Key ->
            %% Found available courier, retrieve the full record.
            [{courier, CourierData}] = ets:lookup(available_couriers, Key),
            CourierID = maps:get(id, CourierData),

            %% Prepare job data to send to the courier process.
            JobData = #{
                order_id => OrderID,
                business_location => PickupLocation,
                customer_location => {100, 100} % מיקום לקוח זמני
            },

            %% Call the courier process to assign the job.
            case gen_statem:call(CourierID, {assign_job, JobData}, 5000) of
                ok ->
                    %% Job assigned successfully
                    ets:delete(available_couriers, Key), % Remove the courier from the available table.
                    ets:insert(busy_couriers, {courier, CourierData, OrderID}),  % Add the courier to the busy table.

                    io:format("Assigned courier ~p to order ~p~n", [CourierID, OrderID]),
                    Reply = {ok, CourierID},
                    {reply, Reply, State};

                {error, Reason} ->
                    %% If the courier did not respond or rejected the task
                    io:format("Failed to assign job to courier ~p: ~p~n", [CourierID, Reason]),
                    Reply = {error, {failed_to_assign, CourierID}},
                    {reply, Reply, State}
            end
    end;



handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.








%% Handles asynchronous calls (gen_server:cast).
handle_cast({release_courier, CourierID}, State) ->
    io:format("Received release request for courier ~p~n", [CourierID]),

    %% TODO: לממש את הלוגיקה של שחרור שליח.
    %% 1. למחוק את השליח מטבלת busy_couriers.
    %% 2. להוסיף אותו בחזרה לטבלת available_couriers.

    {noreply, State}; 

handle_cast(_Msg, State) ->
    {noreply, State}.


%% Handles all other messages sent to the server.
handle_info(_Info, State) ->
    {noreply, State}.


%% Called when the server is shutting down. Allows for cleanup actions.
terminate(_Reason, _State) ->
    ok.


%% Called during hot code upgrades.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.