%%%-------------------------------------------------------------------
%% order_generator - gen_server responsible for creating new orders
%%%-------------------------------------------------------------------
-module(order_generator).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(GENERATION_INTERVAL_MS, 5000). % 5 seconds


%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% gen_server Callbacks
init([]) ->
    io:format("Order generator started.~n"),
    %% Start the timer to generate the first order immediately.
     %erlang:send_after(0, self(), generate_order),
    {ok, #{order_counter => 0}}. % Number of orders generated.



handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


%% Generate a new order
handle_info(generate_order, State = #{order_counter := Counter}) ->
    NewCounter = Counter + 1,
    NewOrderID = erlang:unique_integer([positive]), % Create a unique order ID

    %% Generate random locations for business and customer
    BusinessLocation = {rand:uniform(100), rand:uniform(100)},
    CustomerLocation = {rand:uniform(100), rand:uniform(100)},
    
    % Generate order data with business and customer locations
    OrderData = #{
        id => NewOrderID,
        business_location => BusinessLocation,
        customer_location => CustomerLocation
    },

    io:format("Order generator: Creating order ~p (Business: ~p, Customer: ~p)~n", 
              [NewOrderID, BusinessLocation, CustomerLocation]),

    %% Send an asynchronous message to the "courier_manager" to add the new order to its queue.
    courier_manager:add_to_queue(OrderData),

    %% Restart the timer to generate another order in 5 seconds.
    erlang:send_after(?GENERATION_INTERVAL_MS, self(), generate_order),
    
    {noreply, State#{order_counter => NewCounter}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.