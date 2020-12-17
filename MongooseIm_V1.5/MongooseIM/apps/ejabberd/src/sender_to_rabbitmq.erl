-module(sender_to_rabbitmq).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_rabbitmq.hrl").

-export([publish_message/3]).
-export([send_room_presences_online/4
		, create_producer/2
		, destroy_room/2]).

-define(RABBITMQ_POOL, manager_pool).
-define(MOD_RABBITMQ, mod_rabbitmq).

send_room_presences_online(Host, Content, RoomJID, QueueSender) ->
    Payload = [{<<"type">>, <<"presence">>}, 
            {<<"option">>, ?PRESENCE_ONLINE}, 
			{<<"roomjid">>, RoomJID},
            {<<"memberlist">>, Content}],
    publish_message(Host, Payload, QueueSender).

publish_message(Host, Payload, QueueSender) ->
	BPayload = jsx:encode(Payload),
	?RABBITMQ_POOL:wcast(Host, mod_rabbitmq_connection, 
				{publish_message, {BPayload, QueueSender}}, next_worker).

%% This functions will create 
%%	- Channel
%%  - Queue
%%  - Exchange
%%	- Binding exchange and queue
create_producer(RoomJID, Host) ->
	?RABBITMQ_POOL:wcast(Host, mod_rabbitmq_connection, 
					{create_producer, RoomJID}, next_worker).	

destroy_room(RoomJID, Host) ->
	    ?RABBITMQ_POOL:wcast(Host, mod_rabbitmq_connection,
                    {destroy_room, RoomJID}, next_worker).


