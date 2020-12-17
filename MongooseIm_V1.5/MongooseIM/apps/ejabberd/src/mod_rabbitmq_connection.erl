-module(mod_rabbitmq_connection).

-behaviour(gen_mod).
-behaviour(gen_server).

-include("mod_rabbitmq.hrl").

-record(state, {connection, pool_name, host}).

%% gen_mod 
-export([start/2, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([init_message_broker_hook/2
		, destroy_queue_hook/2
		, presence_online_hook/4]).

-define(CHANNEL_POOL, manager_pool).
-define(SUB_PROCESS, mod_rabbitmq_channel).

-import(mod_rabbitmq, [open_connection/1
					, close_connection/1]).

timestamp({Mega, Secs, Micro}) ->
    Mega * 1000000 * 1000000 + Secs * 1000000 + Micro.

start(Host, Opts) ->
	%% Create hook to declare exchange and queue
    ConferenceHost = gen_mod:get_opt_host(Host, Opts, <<"conference.@HOST@">>),
    ejabberd_hooks:add(init_message_broker_hook, ConferenceHost,
                                ?MODULE, init_message_broker_hook, 75),
    ejabberd_hooks:add(destroy_queue_hook, ConferenceHost,
                                ?MODULE, destroy_queue_hook, 75),
    ejabberd_hooks:add(presence_online_hook, ConferenceHost,
                                ?MODULE, presence_online_hook, 75),
    %% create a workers pool
    ?CHANNEL_POOL:ensure_started(amqp_client),
    ?CHANNEL_POOL:start_link(?MODULE, Host, Opts).
    
stop(Host) ->  
    %% MUC host.
    ConferenceHost = gen_mod:get_module_opt_host(
				Host, ?MODULE, <<"conference.@HOST@">>),
    ejabberd_hooks:delete(init_message_broker_hook, ConferenceHost,
                                ?MODULE, init_message_broker_hook, 75),
    ejabberd_hooks:delete(destroy_queue_hook, ConferenceHost,
                                ?MODULE, destroy_queue_hook, 75),
    ejabberd_hooks:delete(presence_online_hook, ConferenceHost,
                                ?MODULE, presence_online_hook, 75),
    ok.

init([Host, Opts]) ->
    %% Start a connection to the server
    Connection = case mod_rabbitmq:open_connection(Opts) of 
                    {ok, Conn} -> 
                      Conn;
                    undefined -> 
                      undefined 
                end,
	%% create a pool channel for this connection.
	PoolName = list_to_atom( atom_to_list(?SUB_PROCESS) ++ "_" ++ integer_to_list(timestamp(os:timestamp())) ),
	?CHANNEL_POOL:start_link(?SUB_PROCESS, Host, Opts, {PoolName, Connection}),
    {ok, #state{connection = Connection, 
				pool_name = PoolName, 
				host = Host}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({publish_message, {BPayload, QueueSender}}, 
				#state{pool_name = PoolName, host = Host} = State) ->
	?CHANNEL_POOL:wcast(Host, PoolName, 
						{publish_message, {BPayload, QueueSender}}, next_worker),
	{noreply, State};

handle_cast({create_producer, RoomJID}, 
				#state{pool_name = PoolName, host = Host} = State) ->
	?CHANNEL_POOL:wcast(Host, PoolName, {create_producer, RoomJID}, next_worker),
	{noreply, State};

handle_cast({create_consumer, RoomJID}, 
				#state{pool_name = PoolName, host = Host} = State) ->
    ?CHANNEL_POOL:wcast(Host, PoolName, {create_producer, RoomJID}, next_worker),
    {noreply, State};

handle_cast({destroy_room, RoomJID},
                #state{pool_name = PoolName, host = Host} = State) ->
    ?CHANNEL_POOL:wcast(Host, PoolName, {destroy_room, RoomJID}, next_worker),
    {noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({publish_message, {BPayload, QueueSender}}, 
				#state{pool_name = PoolName, host = Host} = State) ->
    ?CHANNEL_POOL:wcast(Host, PoolName, 
						{publish_message, {BPayload, QueueSender}}, next_worker),
    {noreply, State};

handle_info({create_producer, RoomJID}, 
				#state{pool_name = PoolName, host = Host} = State) ->
    ?CHANNEL_POOL:wcast(Host, PoolName, {create_producer, RoomJID}, next_worker),
    {noreply, State};

handle_info(Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, #state{connection = Connection}) ->
    close_connection(Connection).
   
init_message_broker_hook(RoomJID, ServerHost) ->
	sender_to_rabbitmq:create_producer(RoomJID, ServerHost).

destroy_queue_hook(RoomJID, ServerHost) ->
	sender_to_rabbitmq:destroy_room(RoomJID, ServerHost).

presence_online_hook(Service, ListMember, RoomJID, QueueSender) ->
	sender_to_rabbitmq:send_room_presences_online(
				Service, ListMember, RoomJID, QueueSender). 
