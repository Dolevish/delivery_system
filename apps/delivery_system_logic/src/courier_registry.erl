%%%-------------------------------------------------------------------
%% @doc courier_registry - Central registry for courier processes
%%      Ensures synchronization between courier_manager and courier_process_sup
%% @end
%%%-------------------------------------------------------------------
-module(courier_registry).
-behaviour(gen_server).

%% API
-export([start_link/1, register_courier/2, unregister_courier/1, 
         get_all_couriers/0, get_courier_pid/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API
%% ===================================================================

start_link(InitialCouriers) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, InitialCouriers, []).

%% Register a courier process
register_courier(CourierID, Pid) ->
    gen_server:call(?SERVER, {register, CourierID, Pid}).

%% Unregister a courier
unregister_courier(CourierID) ->
    gen_server:call(?SERVER, {unregister, CourierID}).

%% Get all registered couriers
get_all_couriers() ->
    gen_server:call(?SERVER, get_all).

%% Get PID of a specific courier
get_courier_pid(CourierID) ->
    gen_server:call(?SERVER, {get_pid, CourierID}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(InitialCouriers) ->
    %% Create ETS table for courier registry
    ets:new(courier_registry_table, [set, named_table, protected]),

    %% Monitor map for tracking courier process monitors
    Monitors = #{},
    
    {ok, #{couriers => InitialCouriers, monitors => Monitors}}.

handle_call({register, CourierID, Pid}, _From, State = #{monitors := Monitors}) ->
    %% Store in ETS table
    ets:insert(courier_registry_table, {CourierID, Pid}),

    %% Create a monitor for the process
    MonRef = erlang:monitor(process, Pid),
    NewMonitors = maps:put(MonRef, CourierID, Monitors),
    
    io:format("Courier ~p registered with PID ~p~n", [CourierID, Pid]),
    
    {reply, ok, State#{monitors => NewMonitors}};

handle_call({unregister, CourierID}, _From, State = #{monitors := Monitors}) ->
    %% Remove from ETS table
    case ets:lookup(courier_registry_table, CourierID) of
        [{CourierID, _Pid}] ->
            ets:delete(courier_registry_table, CourierID),

            %% Cancel monitor if it exists
            NewMonitors = case find_monitor_by_courier(CourierID, Monitors) of
                {ok, MonRef} ->
                    erlang:demonitor(MonRef, [flush]),
                    maps:remove(MonRef, Monitors);
                not_found ->
                    Monitors
            end,
            
            io:format("Courier ~p unregistered~n", [CourierID]),
            {reply, ok, State#{monitors => NewMonitors}};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_all, _From, State) ->
    AllCouriers = ets:tab2list(courier_registry_table),
    {reply, AllCouriers, State};

handle_call({get_pid, CourierID}, _From, State) ->
    case ets:lookup(courier_registry_table, CourierID) of
        [{CourierID, Pid}] -> {reply, {ok, Pid}, State};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Handle DOWN messages from monitored courier processes
handle_info({'DOWN', MonRef, process, _Pid, Reason}, State = #{monitors := Monitors}) ->
    case maps:get(MonRef, Monitors, undefined) of
        undefined ->
            {noreply, State};
        CourierID ->
            io:format("Courier ~p process died: ~p~n", [CourierID, Reason]),

            %% Remove from registry
            ets:delete(courier_registry_table, CourierID),

            %% Remove monitor
            NewMonitors = maps:remove(MonRef, Monitors),
            
            {noreply, State#{monitors => NewMonitors}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
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