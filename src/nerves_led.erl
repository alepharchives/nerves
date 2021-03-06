%%%-------------------------------------------------------------------
%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2012, Frank Hunleth
%%% @doc
%%%
%%% @end
%%% Created : 20 Feb 2012 by Frank Hunleth <fhunleth@troodon-software.com>
%%%-------------------------------------------------------------------
-module('nerves_led').

-behaviour(gen_server).

%% API
-export([start_link/1,
	 set_brightness/2,
	 brightness/1,
	 max_brightness/1,
	 disable_triggers/1,
	 blink/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(LED_SYSFS_DIR, "/sys/class/leds/").

-record(state, {
	  %% LED class directory path
	  dir_name,
	  
	  %% Handles
	  brightness_file_handle
	 }).


%%%===================================================================
%%% API
%%%===================================================================

%% @doc Change the brightness of the LED. For many LEDs this just 
%%      controls whether they are on (1) or off (0)
set_brightness(Led, BrightnessLevel) ->
    gen_server:call(Led, {set_brightness, BrightnessLevel}).

%% @doc Return the current LED brightness
brightness(Led) ->
    gen_server:call(Led, {brightness}).

%% @doc Get the maximum brightness that may be passed to 
%%      set_brightness/2.
max_brightness(Led) ->
    gen_server:call(Led, {max_brightness}).

%% @doc Disable all triggers on the LED    
disable_triggers(Led) ->
    gen_server:call(Led, {disable_triggers}).

%% @doc Configure a trigger to blink the LED
blink(Led, OnTimeMillis, OffTimeMillis) ->
    gen_server:call(Led, {blink, OnTimeMillis, OffTimeMillis}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(LedName) ->
    gen_server:start_link(?MODULE, [LedName], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([LedName]) ->
    LedDirName = ?LED_SYSFS_DIR ++ LedName ++ "/",
    BrightnessFile = LedDirName ++ "brightness",
    {ok, BrightnessFileHandle} = file:open(BrightnessFile, [read, write]),
    State = #state{dir_name = LedDirName, 
		   brightness_file_handle = BrightnessFileHandle},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({set_brightness, BrightnessLevel}, _From, State) ->
    file:pwrite(State#state.brightness_file_handle, 
		0, 
		integer_to_list(BrightnessLevel)),
    Reply = ok,
    {reply, Reply, State};

handle_call({brightness}, _From, State) ->
    {ok, ValueAsString} = file:pread(State#state.brightness_file_handle, 
				     0, 32),
    {Value, _} = string:to_integer(ValueAsString),
    Reply = {ok, Value},
    {reply, Reply, State};
    
handle_call({max_brightness}, _From, State) ->
    Value = read_sysfs_integer(State#state.dir_name ++ "max_brightness"),
    Reply = {ok, Value},
    {reply, Reply, State};

handle_call({disable_triggers}, _From, State) ->
    write_sysfs_string(State#state.dir_name ++ "trigger", "none"),
    {reply, ok, State};

handle_call({blink, OnTimeMillis, OffTimeMillis}, _From, State) ->
    write_sysfs_string(State#state.dir_name ++ "trigger", "timer"),
    write_sysfs_string(State#state.dir_name ++ "delay_on", integer_to_list(OnTimeMillis)),
    write_sysfs_string(State#state.dir_name ++ "delay_off", integer_to_list(OffTimeMillis)),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
read_sysfs_integer(Filename) ->
    {ok, Handle} = file:open(Filename, [read]),
    {ok, ValueAsString} = file:pread(Handle, 0, 32),
    file:close(Handle),
    {Value, _} = string:to_integer(ValueAsString),
    Value.

write_sysfs_string(Filename, Value) ->
    {ok, Handle} = file:open(Filename, [write]),
    file:pwrite(Handle, 0, Value),
    file:close(Handle).
