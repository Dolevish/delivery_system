%%%-------------------------------------------------------------------
%% @doc graphics_server module. responsible for managing the settings GUI window.
%% @end
%%%-------------------------------------------------------------------
-module(graphics_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("wx/include/wx.hrl").

-record(state, {
    frame,
    num_couriers_input,
    interval_input,
    start_button
}).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    wx:new(),
    
    %% Create main frame
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Delivery System Settings", 
                       [{size, {350, 200}}, {style, ?wxDEFAULT_FRAME_STYLE band bnot ?wxRESIZE_BORDER}]),
    
    %% Create panel
    Panel = wxPanel:new(Frame),
    
    %% Create sizer for layout
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    
    %% Number of couriers setting
    CouriersSizer = wxBoxSizer:new(?wxHORIZONTAL),
    CouriersLabel = wxStaticText:new(Panel, ?wxID_ANY, "Number of Couriers:"),
    NumCouriersInput = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "3"}]),
    wxSizer:add(CouriersSizer, CouriersLabel, [{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxRIGHT}, {border, 10}]),
    wxSizer:add(CouriersSizer, NumCouriersInput, [{proportion, 1}]),
    
    %% Order interval setting
    IntervalSizer = wxBoxSizer:new(?wxHORIZONTAL),
    IntervalLabel = wxStaticText:new(Panel, ?wxID_ANY, "Order Interval (seconds):"),
    IntervalInput = wxTextCtrl:new(Panel, ?wxID_ANY, [{value, "5"}]),
    wxSizer:add(IntervalSizer, IntervalLabel, [{flag, ?wxALIGN_CENTER_VERTICAL bor ?wxRIGHT}, {border, 10}]),
    wxSizer:add(IntervalSizer, IntervalInput, [{proportion, 1}]),
    
    %% Start button
    StartButton = wxButton:new(Panel, ?wxID_ANY, [{label, "Start"}]),
    
    %% Add everything to main sizer
    wxSizer:add(MainSizer, CouriersSizer, [{flag, ?wxEXPAND bor ?wxALL}, {border, 10}]),
    wxSizer:add(MainSizer, IntervalSizer, [{flag, ?wxEXPAND bor ?wxLEFT bor ?wxRIGHT bor ?wxBOTTOM}, {border, 10}]),
    wxSizer:add(MainSizer, StartButton, [{flag, ?wxALIGN_CENTER bor ?wxALL}, {border, 10}]),
    
    %% Set sizer for panel
    wxPanel:setSizer(Panel, MainSizer),
    
    %% Connect button event
    wxButton:connect(StartButton, command_button_clicked),
    
    %% Show frame
    wxFrame:center(Frame),
    wxFrame:show(Frame),
    
    State = #state{
        frame = Frame,
        num_couriers_input = NumCouriersInput,
        interval_input = IntervalInput,
        start_button = StartButton
    },
    
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(#wx{event = #wxCommand{type = command_button_clicked}}, 
            State = #state{num_couriers_input = NumCouriersInput, 
                          interval_input = IntervalInput,
                          start_button = StartButton}) ->
    
    %% Get values from inputs
    NumCouriersStr = wxTextCtrl:getValue(NumCouriersInput),
    IntervalStr = wxTextCtrl:getValue(IntervalInput),
    
    %% Parse values
    case {string:to_integer(NumCouriersStr), string:to_integer(IntervalStr)} of
        {{NumCouriers, ""}, {IntervalSec, ""}} when NumCouriers > 0, IntervalSec > 0 ->
            io:format("Starting system with ~p couriers and ~p seconds interval~n", 
                     [NumCouriers, IntervalSec]),
            
            %% Disable button
            wxButton:disable(StartButton),
            wxButton:setLabel(StartButton, "Run"),
            
            %% Send configuration to the system
            config_manager:set_config(NumCouriers, IntervalSec * 1000),
            
            {noreply, State};
        _ ->
            %% Invalid input
            wxMessageDialog:showModal(wxMessageDialog:new(State#state.frame, 
                                                         "Please enter valid positive numbers",
                                                         [{caption, "Invalid Input"},
                                                          {style, ?wxOK bor ?wxICON_ERROR}])),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    wx:destroy(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.