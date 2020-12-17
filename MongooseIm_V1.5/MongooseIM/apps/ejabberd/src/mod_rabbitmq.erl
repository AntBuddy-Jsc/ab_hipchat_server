-module(mod_rabbitmq).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_rabbitmq.hrl").

-export([open_connection/1,
		open_channel/1,
		close_connection/1, 
		close_connection/2,
		close_channel/1,
		exchange_declare/3, 
		queue_declare/2, 
		queue_bind/4, 
		basic_consume/2,
		basic_ack/2,
		publish_message/4,
		bind_queue_exchange/4,
		consume_queue/2,
        get_channel_exchange/1,
		queue_delete/2,
		exchange_delete/2,
		queue_unbind/4,
		destroy_queue/2]).


get_config(Opts, ConfKeyList) ->
    Fun =
        fun(Key) ->
                case lists:keyfind(list_to_atom(Key), 1, Opts) of
                    false ->
                        undefined;
                    {_,Val} ->
                        Val
                end
        end,
    lists:map(Fun, ConfKeyList).

open_connection(Opts) ->
	[MQ_VHost, MQ_host, MQ_port, MQ_login, MQ_password, MQ_heartbeat] =
        get_config(Opts, ["vhost", "mqhost", "port", "login", "password", "heartbeat"]),
    Single_host = io_lib:printable_list(MQ_host),
	MQ_host2 = if
        Single_host -> MQ_host;
        true ->
            <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
            random:seed(A, B, C),
            lists:nth(random:uniform_s(length(MQ_host), os:timestamp()), MQ_host)
    end,

    AMQP_params = #amqp_params_network{
        virtual_host = MQ_VHost,
        username = MQ_login,
        password = MQ_password,
        host = MQ_host2,
        port = MQ_port,
        ssl_options = none,
        heartbeat = MQ_heartbeat
    },
    case catch amqp_connection:start(AMQP_params) of
        {ok, Conn} ->
            ?INFO_MSG("New Connection: ~p~n", [Conn]),
			{ok, Conn};
        ConnEx ->
            ?DEBUG("AB#27 : Connection Exception ~p~n",[ConnEx]),
            undefined
    end.

open_channel(Connection) ->
	case catch amqp_connection:open_channel(Connection) of
		{ok, Chan} ->
			Chan;
		ChanEx ->
			?ERROR_MSG("AB#27 : Channel Exception ~p~n",[ChanEx]),
			undefined
	end.

close_channel(Channel) ->
	amqp_channel:close(Channel).

close_connection(Connection) ->
    amqp_connection:close(Connection).

close_connection(Connection, Channel) ->
	amqp_channel:close(Channel),
    amqp_connection:close(Connection).

queue_declare(Channel, QueueName) ->
	Declare = #'queue.declare'{queue= QueueName,durable=true},
	case catch amqp_channel:call(Channel, Declare) of
		#'queue.declare_ok'{} ->
			Declare;
		Reason ->
			?ERROR_MSG("AB#27: Reason ~p~n QueueName ~p~n", [Reason, QueueName]),
			error
	end.

queue_delete(Channel, Queue) ->
	Delete = #'queue.delete'{queue = Queue},
	case amqp_channel:call(Channel, Delete) of 
		#'queue.delete_ok'{} -> ok;
		Reason ->
				?ERROR_MSG("Reason ~p~n", [Reason]),
				ok
	end.
	
exchange_declare(Channel, ExchangeName, Type) ->
	Declare = #'exchange.declare'{
        exchange = ExchangeName,
        type = Type,
        durable = true
    },
	case catch amqp_channel:call(Channel, Declare) of
        #'exchange.declare_ok'{} ->
            Declare;
        Reason ->
            ?ERROR_MSG("AB#27: Reason ~p~n Exchange ~p~n", [Reason, ExchangeName]),
            error
    end.

exchange_delete(Channel, Exchange) ->
    Delete = #'exchange.delete'{exchange = Exchange},
    case amqp_channel:call(Channel, Delete) of
		#'exchange.delete_ok'{} -> ok;
		Reason -> 
			?ERROR_MSG("Reason ~p~n", [Reason]),
			ok
	end.

queue_bind(_, [], [], _) ->
	error;

queue_bind(_, [], _, _) ->
    error;

queue_bind(_, _ , [], _) ->
    error;

queue_bind(Channel, Queue, Exchange, RK) ->
	Binding = #'queue.bind'{
        queue = Queue,
        exchange = Exchange,
        routing_key = RK
    },
	case catch amqp_channel:call(Channel, Binding) of
        #'queue.bind_ok'{} ->
            Binding;
        Reason ->
            ?ERROR_MSG("AB#27: Reason ~p~n", [Reason]),
            error
    end.

queue_unbind(Channel, Queue, Exchange, RK) ->
	Binding = #'queue.unbind'{
        queue = Queue,
        exchange = Exchange,
        routing_key = RK
    },
    case catch amqp_channel:call(Channel, Binding) of
        #'queue.unbind_ok'{} -> ok;
        Reason ->
            ?ERROR_MSG("AB#27: Reason ~p~n", [Reason]),
            error
    end.

		
%% The caller is the subscriber
basic_consume(<<>>, _Queue) ->
    error;

basic_consume(_Channel, []) ->
    error;

basic_consume(Channel, Queue) ->
	#'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:call(Channel, #'basic.consume'{
        queue = Queue
    }),
	Tag.

publish_message(undefined, _, _, _) ->
	%?INFO_MSG("AB#27: Channel empty ~n",  []),
	ok;

publish_message(_, _, _, <<>>) ->
	?INFO_MSG("AB#27: PayLoad empty ~n", []),
    ok;
	
publish_message(Channel, Exchange, Key, BPayload) ->
	Publish = #'basic.publish'{exchange = Exchange, routing_key = Key},
	%AB-84: add time-to-live to each message publish
    Props = #'P_basic'{delivery_mode = 2, expiration = ?PUSH_MSG_TTL}, %% persistent message
    Msg = #amqp_msg{props = Props, payload = BPayload},
    amqp_channel:cast(Channel, Publish, Msg).

destroy_queue(Channel, RoomJID) ->
	{Queue, Exchange, RK} = create_name_queue_exchange_rk(RoomJID),
	queue_unbind(Channel, Queue, Exchange, RK),
	exchange_delete(Channel, Exchange),
	queue_delete(Channel, Queue).

%% Send respond message was sent successfully
basic_ack(Channel, Tag) ->
	amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}).

bind_queue_exchange(Channel, RoomJID, Type, _Connection) ->
	{QueueName, ExchangeName, RK} = create_name_queue_exchange_rk(RoomJID),
	Exchange = case exchange_declare(Channel, ExchangeName, Type) of
        error -> [];
        _ -> ExchangeName
    end,
	Queue = case queue_declare(Channel, QueueName) of
		error -> [];
		_ -> QueueName
	end,
	case queue_bind(Channel, Queue, Exchange, RK)  of
		error -> 
			error;
		Binding ->
			Binding
	end.

consume_queue(Channel, RoomJID) ->
	QueueName = <<?QUEUE_AB/binary, ".", RoomJID/binary>>,
    Queue = case queue_declare(Channel, QueueName) of
        error -> [];
        _ -> QueueName
    end,
	amqp_channel:call(Channel, #'basic.qos'{prefetch_count = ?PREFETCH_COUNT}),
    basic_consume(Channel, Queue).

create_name_queue_exchange_rk(RoomJID) ->
	Queue = <<?QUEUE_AB/binary, ".", RoomJID/binary>>,
	{Exchange, RoutingKey} = get_channel_exchange(RoomJID),
	{Queue, Exchange, RoutingKey}.

get_channel_exchange(RoomJID) ->
	{<<?EXCHANGE_AB/binary, ".", RoomJID/binary>>,
	<<?ROUTING_KEY_AB/binary, ".", RoomJID/binary>>}.
