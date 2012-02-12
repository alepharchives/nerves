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
-export([open/1, set_brightness/2, close/1]).

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

close(Led) ->
    file:close(Led#led_info.brightness_file_handle).

%%%===================================================================
%%% Internal functions
%%%===================================================================
