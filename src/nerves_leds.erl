%%%-------------------------------------------------------------------
%%% @author Frank Hunleth <fhunleth@troodon-software.com>
%%% @copyright (C) 2012, Frank Hunleth
%%% @doc
%%%
%%% @end
%%% Created : 12 Feb 2012 by Frank Hunleth <fhunleth@troodon-software.com>
%%%-------------------------------------------------------------------
-module('nerves_leds').


%% Internal state

-define(LED_SYSFS_DIR, "/sys/class/leds/").

-record(led_info, {
	  %% LED class directory path
	  dir_name,
	  
	  %% Handles
	  brightness_file_handle
	 }).

%% API
-export([
	 enumerate/0,
	 open/1, 
	 close/1,
	 set_brightness/2,
	 max_brightness/1,
	 disable_triggers/1,
	 blink/3
	]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Return a list of the LEDs that are controllable through
%%      the Linux LED class driver
enumerate() ->
    {ok, LedNames} = file:list_dir(?LED_SYSFS_DIR),
    LedNames.


%% @doc Open the specified LED for use
open(LedName) ->
    LedDirName = ?LED_SYSFS_DIR ++ LedName ++ "/",
    BrightnessFile = LedDirName ++ "brightness",
    {ok, BrightnessFileHandle} = file:open(BrightnessFile, [read, write]),
    #led_info{dir_name = LedDirName, 
	      brightness_file_handle = BrightnessFileHandle}.

%% @doc Release the resources associated with the LED
close(Led) ->
    file:close(Led#led_info.brightness_file_handle).

%% @doc Change the brightness of the LED. For many LEDs this just 
%%      controls whether they are on (1) or off (0)
set_brightness(Led, BrightnessLevel) ->
    file:pwrite(Led#led_info.brightness_file_handle, 
		0, 
		integer_to_list(BrightnessLevel)).

%% @doc Get the maximum brightness that may be passed to 
%%      set_brightness/2.
max_brightness(Led) ->
    read_sysfs_integer(Led#led_info.dir_name ++ "max_brightness").

%% @doc Disable all triggers on the LED    
disable_triggers(Led) ->
    write_sysfs_string(Led#led_info.dir_name ++ "trigger", "none").

%% @doc Configure a trigger to blink the LED
blink(Led, OnTimeMillis, OffTimeMillis) ->
    write_sysfs_string(Led#led_info.dir_name ++ "trigger", "timer"),
    write_sysfs_string(Led#led_info.dir_name ++ "delay_on", integer_to_list(OnTimeMillis)),
    write_sysfs_string(Led#led_info.dir_name ++ "delay_off", integer_to_list(OffTimeMillis)).

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
    
