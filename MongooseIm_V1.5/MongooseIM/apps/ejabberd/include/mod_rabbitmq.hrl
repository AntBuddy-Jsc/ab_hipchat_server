-ifndef(MONGOOSEIM_MOD_RABBITMQ_HRL).
-define(MONGOOSEIM_MOD_RABBITMQ_HRL, true).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").

%% definition of exchange and queue in mod_rabbitmq
%% Exchange
-define(EXCHANGE_AB,				<<"exchange.abxmpp">>).
-define(EXCHANGE_AB_PUSH,           <<"exchange.abxmpp.push">>).

%% Queue
-define(QUEUE_AB,					<<"queue.abxmpp">>).
-define(QUEUE_AB_PUSH,              <<"queue.abxmpp.push">>).


%% Routing_key
-define(ROUTING_KEY_AB,				<<"routingkey.abxmpp">>).
-define(ROUTING_KEY_AB_PUSH,        <<"routingkey.abxmpp.push">>).


-define(PREFETCH_COUNT, 20).
-define(PRESENCE_ONLINE, <<"presence_online">>).

-define(PUSH_MSG_TTL, <<"3600000">>). %% 1h
% -define(PUSH_MSG_TTL, <<"2000">>). %% 2s

-define(Strategy, next_worker).


-define(MESSAGE_CHANNEL_TABLE, rabbitmq_message_pool).
-endif.
