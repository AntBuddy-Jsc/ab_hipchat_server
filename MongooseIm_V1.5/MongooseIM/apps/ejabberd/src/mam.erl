%% @author bokner
%% @doc Common functions/data for MAM


-module(mam).

%% ====================================================================
%% API functions
%% ====================================================================
-export([resolve_request_params/2, ts/0, forwarded/1]).
-export([timestamp_ms/1]).
-export([mamts_to_timestamp/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mam.hrl").
-include_lib("exml/include/exml.hrl").

%% Compile the page request from the MAM request
resolve_request_params(From, Query) ->
  WithJid = 
	case xml:get_subtag_cdata(Query, <<"with">>) of
	  <<>> ->
		From;
	  JidStr ->
		jlib:binary_to_jid(JidStr)
	end,
  StartTs = 
	case xml:get_subtag_cdata(Query, <<"start">>) of
	  <<>> ->
		?ZERO_TIMESTAMP;
	  S ->
		timestamp_ms(S)
	end,
  EndTs = 
	case xml:get_subtag_cdata(Query, <<"end">>) of
	  <<>> ->
		?INFINITE_TIMESTAMP;
	  E ->
		timestamp_ms(E)
	end,
  Rsm = jlib:rsm_decode(Query),
  Limit = case catch Rsm#rsm_in.max of
			undefined ->
			  gen_mod:get_module_opt(global, mod_muc, history_size, ?DEFAULT_PAGE_SIZE);
			{'EXIT', Err} -> ?INFO_MSG("rsm_in.max: ~p~n", [Err]), ?DEFAULT_PAGE_SIZE;
			Max ->
			  Max
		  end,
  
  %% Resolve RSM into start and end
  {Start, End, Direction} =
	case catch Rsm#rsm_in.direction of 
			  aft ->
				{case Rsm#rsm_in.id of [] -> StartTs; Ts1 -> timestamp_ms(Ts1) end + 1, EndTs, forward};
	  		  before ->
				{StartTs, case Rsm#rsm_in.id of [] -> EndTs; Ts2 -> timestamp_ms(Ts2) end - 1, backward};
	  		  undefined ->
				{StartTs, EndTs, forward};
			  {'EXIT', Reason} -> 
			  	?INFO_MSG("rsm_in.direction: ~p~n", [Reason]),
			  	{StartTs, EndTs, forward}
	end,
  {WithJid, Start, End, Limit, Direction}.

ts() ->
    {Mega, Secs, Micro} = os:timestamp(),
%	trunc((Mega * 1000000 * 1000000 + Secs * 1000000 + Micro)/1000). 
    Mega * 1000000 * 1000000 + Secs * 1000000 + Micro.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% Timestamp in milliseconds
timestamp_ms(DateString) when is_list(DateString) ->
	{Mega, Secs, Micro} = case jlib:datetime_string_to_timestamp(DateString) of
		undefined -> os:timestamp();
		T -> T
	end,
	trunc((Mega * 1000000 * 1000000 + Secs * 1000000 + Micro)/1000);

%% TODO: update jlib functions to work with binaries
timestamp_ms(DateString) when is_binary(DateString) ->
	timestamp_ms(binary_to_list(DateString)).


%% Timestamp to UTC
timestamp_to_utc(Ts) ->
	MegaSecs = Ts div 1000000000,
	Rem1 = Ts - MegaSecs * 1000000000,
	Secs = Rem1 div 1000,
	MicroSecs = (Rem1 - Secs * 1000) * 1000,
	jlib:now_to_utc_string({MegaSecs, Secs, MicroSecs}).


%% Originally taken from mod_mam_utils
forwarded(Packet) ->
    #xmlel{
        name = <<"forwarded">>,
        attrs = [{<<"xmlns">>, <<"urn:xmpp:forward:0">>}],
        children = [Packet]}.

%% get timestamp from mamts
mamts_to_timestamp(Ts) ->
	MegaSecs = Ts div 1000000000,
	Rem1 = Ts - MegaSecs * 1000000000,
	Secs = Rem1 div 1000,
	MicroSecs = (Rem1 - Secs * 1000) * 1000,
	{MegaSecs, Secs, MicroSecs}.
