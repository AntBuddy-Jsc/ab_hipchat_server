-module(mod_bot_action).
-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").

%% API
-export([start/2, stop/1]).

-export([	add_new_user/3, 
			remove_user/3,
			destroy_room/3,
			send_messages/4
		]).

start(Host, Opts) ->
	ejabberd_hooks:add(bot_add_new_user, Host, ?MODULE, add_new_user, 50),
	ejabberd_hooks:add(bot_remove_user, Host, ?MODULE, remove_user, 50),
	ejabberd_hooks:add(bot_destroy_room, Host, ?MODULE, destroy_room, 50),
	ejabberd_hooks:add(bot_send_message, Host, ?MODULE, send_messages, 50).

stop(Host) ->
	ejabberd_hooks:delete(bot_add_new_user, Host, ?MODULE, add_new_user, 50),
	ejabberd_hooks:delete(bot_remove_user, Host, ?MODULE, remove_user, 50),
    ejabberd_hooks:delete(bot_destroy_room, Host, ?MODULE, destroy_room, 50),
	ejabberd_hooks:delete(bot_send_message, Host, ?MODULE, send_messages, 50).

add_new_user(From, RoomId, Timestamp) ->
	Doc = [{userid, From#jid.luser},
			{groupid, RoomId},
			{action, <<"add_new_user">>}, 
			{timestamp, Timestamp}],
	mod_mongo:insert_bot(Doc).

remove_user(JID, RoomId, Timestamp) ->
	Doc = [{userid, JID#jid.luser},
            {groupid, RoomId},
			{action, <<"remove_user">>},
			{timestamp, Timestamp}],
	mod_mongo:insert_bot(Doc).

destroy_room(From, RoomId, Timestamp) ->
	?INFO_MSG("GO here From ~p~n", [From]),
	Doc = [{userid, From#jid.luser},
            {groupid, RoomId},
			{action, <<"destroy_room">>},
			{timestamp, Timestamp}],
	mod_mongo:insert_bot(Doc).

send_messages(From, RoomId, Packet, Timestamp) ->
	?INFO_MSG("GO here Packet ~p~n", [Packet]),
    Doc = [{userid, From#jid.luser},
            {groupid, RoomId},
            {action, <<"messages">>},
            {timestamp, Timestamp}],
    mod_mongo:insert_bot(Doc).
