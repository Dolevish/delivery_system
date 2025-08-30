%%%-------------------------------------------------------------------
%% order_generator - gen_server responsible for creating new orders
%%%-------------------------------------------------------------------
-module(order_generator).
-behaviour(gen_server).

-export([start_link/0, set_interval/1, start_generation/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_GENERATION_INTERVAL_MS, 5000). % 5 seconds default


%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Set the generation interval
set_interval(IntervalMs) ->
    gen_server:cast(?SERVER, {set_interval, IntervalMs}).

%% Start generating orders
start_generation() ->
    gen_server:cast(?SERVER, start_generation).



%% gen_server Callbacks
init([]) ->
    io:format("Order generator started, waiting for configuration...~n"),
    %% Don't start generating orders immediately
    {ok, #{order_counter => 0, interval_ms => ?DEFAULT_GENERATION_INTERVAL_MS, is_running => false}}.



handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_interval, IntervalMs}, State) ->
    io:format("Order generator: Setting interval to ~p ms~n", [IntervalMs]),
    {noreply, State#{interval_ms => IntervalMs}};

handle_cast(start_generation, State = #{is_running := false}) ->
    io:format("Order generator: Starting order generation~n"),
    %% Start generating orders
    erlang:send_after(0, self(), generate_order),
    {noreply, State#{is_running => true}};

handle_cast(start_generation, State) ->
    %% Already running
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.



%% Generate a new order
handle_info(generate_order, State = #{order_counter := Counter, interval_ms := IntervalMs, is_running := true}) ->
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

    %% Restart the timer to generate another order after the configured interval.
    erlang:send_after(IntervalMs, self(), generate_order),
    
    {noreply, State#{order_counter => NewCounter}};

handle_info(generate_order, State) ->
    %% Not running, ignore
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.