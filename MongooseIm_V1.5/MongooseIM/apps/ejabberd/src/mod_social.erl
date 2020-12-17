
-module(mod_social).
-behaviour(gen_mod).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
-include("ejabberd.hrl").

-define(LISTENER, ?MODULE).

start(Host, Opts) ->
	MessagePrefix = proplists:get_value(prefix, Opts, "/v3/message"),
	BotPrefix = proplists:get_value(bot_prefix, Opts, "/v3/bots") ++ "/[:bot_id]",
	ViberPrefix = proplists:get_value(viber_prefix, Opts, "/viber/message/[...]"),
	BeeIQPrefix = proplists:get_value(beeiq_prefix, Opts, "/v3/beeiq") ++ "/[:beeiq_id]",
	Port =	proplists:get_value(port, Opts, 8099),
	CbHost = proplists:get_value(host, Opts, {127,0,0,1}),
	Dispatch = cowboy_router:compile([
		{'_', [
			{MessagePrefix, mod_social_handler, [Host, Opts]},
			{BotPrefix, mod_social_bot_handler, [Host, Opts]},
			{ViberPrefix, mod_viber_bot_handler, [Host, Opts]},
			{BeeIQPrefix, mod_beeiq_handler, [Host, Opts]}
		]}
	]),
	mod_social_service:start_link(),
	mod_social_session:start(),
	ets:new(track_social_bot, [public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
	ets:new(track_beeiq_customer_room, [public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
	ets:new(track_beeiq_room_kiteappid, [public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
	ets:new(track_beeiq_accesstoken, [public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
	ets:new(track_beeiq_hookurl, [public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
	spawn(fun() ->  mod_social_session:restore_all_session(Opts) end),
	case cowboy:start_http(?LISTENER, 100, [{ip, CbHost}, {port, Port}], [
				{env, [{dispatch, Dispatch}]}]) of 
		{error, {already_started, _Pid}} -> 
			ok;
		{ok, Pid} -> 
			?INFO_MSG("#69 Starting cowboy social success ~p~n", [Pid]),
			ok;
		{error, Reason} ->
			?ERROR_MSG("#69 Cannot start cowboy skype listener: ~p, port: ~p, reason: ~p~n", [?LISTENER, 8099, Reason]),
			{error, Reason}
	end.

stop(_Host) ->
	cowboy:stop_listener(?LISTENER),
	ok.

