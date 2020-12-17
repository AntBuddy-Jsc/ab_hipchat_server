-module(mod_rabbitmq_channel).
-behaviour(gen_server).

-include("mod_rabbitmq.hrl").

-record(state, {connection, channel}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-import(mod_rabbitmq, [open_channel/1
					, get_channel_exchange/2
					, publish_message/4
					, bind_queue_exchange/5
					, close_channel/1]).

init([Host, Opts, Connection]) ->
    %% Start a connection to the server
    Channel = mod_rabbitmq:open_channel(Connection),
    {ok, #state{connection = Connection, channel = Channel}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% publish message via queues of rabbitmq
handle_cast({publish_message, {BPayload, RoomJID}}, #state{channel = Channel} = State) ->
    {Exchange, Key} = mod_rabbitmq:get_channel_exchange(RoomJID),
    mod_rabbitmq:publish_message(Channel, Exchange, Key, BPayload),
    {noreply, State};

handle_cast({create_producer, RoomJID}, State) ->
    mod_rabbitmq:bind_queue_exchange(State#state.channel, 
									RoomJID, <<"direct">>, State#state.connection),
    {noreply, State};

handle_cast({create_consumer, RoomJID}, State) ->
    mod_rabbitmq:consume_queue(State#state.channel, RoomJID),
    {noreply, State};

handle_cast({destroy_room, RoomJID}, State) ->
	mod_rabbitmq:destroy_queue(State#state.channel, RoomJID),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% publish message via queues of rabbitmq
handle_info({publish_message, {BPayload, RoomJID}}, #state{channel = Channel} = State) ->
    {Exchange, Key} = mod_rabbitmq:get_channel_exchange(RoomJID),
    mod_rabbitmq:publish_message(Channel, Exchange, Key, BPayload),
    %for debug
    gen_server:cast(mod_send_message, count),
    {noreply, State};

handle_info({create_producer, RoomJID}, State) ->
    mod_rabbitmq:bind_queue_exchange(State#state.channel,              
                                    RoomJID, <<"direct">>, State#state.connection),
    {noreply, State};

handle_info({#'basic.consume_ok'{}}, State) ->
    ?INFO_MSG("AB#27: basic_consume_ok: Subscribe channel ~n",[]),
    {noreply, State};

handle_info({#'basic.cancel_ok'{}}, State) ->
    ?INFO_MSG("AB#27: basic_cancel: Cancel subscribe this channel ~n",[]),
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = BPayLoad}},
                                        #state{channel = Channel} = State) ->
    ?INFO_MSG("AB#27: Receiving ~n", []),
    mod_rabbitmq:basic_ack(Channel, Tag),
    case is_binary(BPayLoad) of
        true ->
            Jdata = binary_to_term(BPayLoad),
            ?INFO_MSG("AB#27: Receive message ~p~n", [Jdata]),

            %for debug
            gen_server:cast(mod_count_message, count);
        _ ->
            ?INFO_MSG("AB#27: Not Right Format: ~n",[]),
            ok
    end,
    {noreply, State};

handle_info(Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.

terminate(_Reason, #state{channel = Channel}) ->
	?ERROR_MSG("AB#27 Stop Reason ~p~n", [_Reason]),
    mod_rabbitmq:close_channel(Channel).

