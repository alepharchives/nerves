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

-record(led_info, {
	  %% LED class directory path
	  dir_name,
	  
	  %% Handles
	  brightness_file_handle
	 }).

%% API
-export([open/1, 
	 close/1,
	 set_brightness/2,
	 max_brightness/1
	]).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
open(LedName) ->
    LedDirName = "/sys/class/leds/" ++ LedName,
    BrightnessFile = LedDirName ++ "/brightness",
    {ok, BrightnessFileHandle} = file:open(BrightnessFile, [read, write]),
    #led_info{dir_name = LedDirName, 
	      brightness_file_handle = BrightnessFileHandle}.

set_brightness(Led, BrightnessLevel) ->
    file:pwrite(Led#led_info.brightness_file_handle, 
		0, 
		integer_to_list(BrightnessLevel)).

max_brightness(Led) ->
    read_sysfs_integer(Led#led_info.dir_name ++ "/max_brightness").
    
close(Led) ->
    file:close(Led#led_info.brightness_file_handle).

%%%===================================================================
%%% Internal functions
%%%===================================================================
read_sysfs_integer(Filename) ->
    {ok, Handle} = file:open(Filename, [read]),
    {ok, ValueAsString} = file:pread(Handle, 0, 32),
    {Value, _} = string:to_integer(ValueAsString),
    Value.
