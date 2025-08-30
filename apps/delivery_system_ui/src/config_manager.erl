%%%-------------------------------------------------------------------
%% @doc config_manager - Manages system configuration and initialization
%% @end
%%%-------------------------------------------------------------------
-module(config_manager).
-behaviour(gen_server).

-export([start_link/0, set_config/2, get_config/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    num_couriers = 3,
    order_interval_ms = 5000,
    is_configured = false
}).

%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

set_config(NumCouriers, IntervalMs) ->
    gen_server:cast(?SERVER, {set_config, NumCouriers, IntervalMs}).

get_config() ->
    gen_server:call(?SERVER, get_config).

%% gen_server callbacks
init([]) ->
    io:format("Config manager started, waiting for configuration...~n"),
    {ok, #state{}}.

handle_call(get_config, _From, State = #state{is_configured = IsConfigured, 
                                               num_couriers = NumCouriers,
                                               order_interval_ms = IntervalMs}) ->
    Reply = case IsConfigured of
        true -> {ok, NumCouriers, IntervalMs};
        false -> {error, not_configured}
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({set_config, NumCouriers, IntervalMs}, State) ->
    io:format("Config manager: Setting configuration - Couriers: ~p, Interval: ~p ms~n", 
              [NumCouriers, IntervalMs]),
    
    %% Create courier data
    CourierData = [
        #{id => list_to_atom("courier_" ++ integer_to_list(N)), 
          location => {rand:uniform(100), rand:uniform(100)}}
        || N <- lists:seq(1, NumCouriers)
    ],
    
    %% Initialize the logic system with the configuration
    initialize_system(CourierData, IntervalMs),
    
    NewState = State#state{
        num_couriers = NumCouriers,
        order_interval_ms = IntervalMs,
        is_configured = true
    },
    
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
initialize_system(CourierData, IntervalMs) ->
    %% Start the courier manager with courier data
    case whereis(courier_manager) of
        undefined ->
            io:format("Error: courier_manager not found~n");
        _ ->
            %% Initialize couriers
            courier_manager:initialize_couriers(CourierData),
            
            %% Configure order generator with interval
            order_generator:set_interval(IntervalMs),
            
            %% Start order generation
            order_generator:start_generation(),
            
            io:format("System initialized with ~p couriers and ~p ms interval~n", 
                     [length(CourierData), IntervalMs])
    end.