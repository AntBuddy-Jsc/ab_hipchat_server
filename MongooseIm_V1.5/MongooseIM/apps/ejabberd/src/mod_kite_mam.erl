-module(mod_kite_mam).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_antbuddy_stanza.hrl").

%% gen_mod 
-export([start/2, stop/1]).
-export([update_room_status/6
		, count_unread_message_kite_room/2]).

-define(PROCNAME, ejabberd_mod_antbuddy_mam).

start(Host, _Opts) ->
	%% count a number of unread messages of kite room everytime user joins kite room
    ejabberd_hooks:add(count_unread_message_kite_room, Host, ?MODULE, 
    								count_unread_message_kite_room, 75),
	%% hook for update active/inactive status of room
    ejabberd_hooks:add(update_room_status, Host, 
    						?MODULE, update_room_status, 75).

stop(Host) ->
	ejabberd_hooks:delete(count_unread_message_kite_room, Host, ?MODULE, 
    								count_unread_message_kite_room, 75),
	ejabberd_hooks:delete(update_room_status, Host, 
							?MODULE, update_room_status, 75).

% From: User
% Server: kite server
% ObjectJID: Kite room
% Status: active/inactive
% Ts: timestamp
update_room_status(Type, From, Server, ObjectJID, _Status, _Ts) ->
	case {check_server(Server), check_server(From#jid.lserver)}  of
		{true, false} ->
			Stanza = mod_antbuddy_stanza:notify_kite_room_active(ObjectJID, Type),
			Service = jlib:make_jid(<<>>, Server, <<>>),
			ejabberd_router:route(Service, From, Stanza);
		_ ->
			no_need_update_status
	end.

%% check server is a kite server
check_server(Server) ->
	case ejabberd_config:get_local_option({host_type, Server}) of
		support ->
			true;
		_ ->
			false
	end.

count_unread_message_kite_room(From, RoomJID) ->
	?DEBUG("#59 run hook here From:~p, RoomJID:~p~n", [From, RoomJID]),
	Server = case binary:split(RoomJID#jid.lserver, <<".">>) of
				[<<"conference">>, Server_Host] -> 
					Server_Host;
				_ -> 
					RoomJID#jid.lserver
			end,
	BRoomJID = jlib:jid_to_binary(jlib:jid_remove_resource(RoomJID)),
	case mod_active_status_odbc:get_timestamp_status_user(
			Server, From#jid.luser, From#jid.lserver, BRoomJID) of
		{Status, Ts} ->
			mod_antbuddy_mam:get_unread_message(groupchat, From, Server, 
				BRoomJID, Status, Ts, <<>>, support);
		_ ->
			nothing_to_count
	end.
