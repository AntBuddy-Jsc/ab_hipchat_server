-module(mod_rabbitmq_message_channel).
-behaviour(gen_server).

-include("mod_rabbitmq.hrl").

-record(state, {connection, 
                channel,
                channel_ref, 
                message_queue = queue:new()}).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

init([_Host, _Opts, Connection]) ->
    State = open_channel(Connection, #state{}),
    {ok, State}.


handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%% publish message via queues of rabbitmq
handle_cast({publish_message, BPayload}, #state{channel = Channel} = State) when Channel == 'undefined' ->
    New_queue = queue:in({publish_message, BPayload}, State#state.message_queue),
    {noreply, State#state{message_queue = New_queue}};

handle_cast({publish_message, BPayload}, #state{channel = Channel} = State) ->
    {Exchange, Key} = {?EXCHANGE_AB_PUSH, ?ROUTING_KEY_AB_PUSH},
    mod_rabbitmq:publish_message(Channel, Exchange, Key, BPayload),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.



%% publish message via queues of rabbitmq
handle_info({publish_message, BPayload}, #state{channel = Channel} = State) when Channel == 'undefined' ->
    New_queue = queue:in({publish_message, BPayload}, State#state.message_queue),
    {noreply, State#state{message_queue = New_queue}};

handle_info({publish_message, BPayload}, #state{channel = Channel} = State) ->
    {Exchange, Key} = {?EXCHANGE_AB_PUSH, ?ROUTING_KEY_AB_PUSH},
    mod_rabbitmq:publish_message(Channel, Exchange, Key, BPayload),
    {noreply, State};

%% update channel
handle_info({update_channel, Connection}, State) ->
    State0 = open_channel(Connection, State),
    ?DEBUG("AB#65: total queued message: ~p~n", [queue:len(State0#state.message_queue)]), 
    lists:map(fun ({publish_message, BPayload}) -> 
                    mod_rabbitmq:publish_message(State0#state.channel, ?EXCHANGE_AB_PUSH, ?ROUTING_KEY_AB_PUSH, BPayload)
                end, queue:to_list(State0#state.message_queue)),
    {noreply, State0#state{message_queue = queue:new()}}; 

handle_info({'DOWN', _Reference, 'process', _Pid, _Reason}, State) ->
    {noreply, disconnected(State)};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}.

terminate(Reason, #state{channel = Channel}) ->
    ?DEBUG("AB#65 Stop Reason ~p~n", [Reason]),
    mod_rabbitmq:close_channel(Channel).

disconnected(#state{channel_ref = Ref} = State) when is_reference(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    disconnected(State#state{channel_ref = 'undefined'});
disconnected(#state{channel = Channel} = State) when is_pid(Channel) ->
    _ = (catch mod_rabbitmq:close_channel(Channel)),
    disconnected(State#state{channel = 'undefined'});
disconnected(State) -> State.

open_channel(Connection, State) -> 
    ?DEBUG("AB#65: Connection: ~p, State: ~p", [Connection, State]),
    Channel = mod_rabbitmq:open_channel(Connection),
    Ref = erlang:monitor('process', Channel),
    ets:insert(?MESSAGE_CHANNEL_TABLE, {Connection, self()}), 
    State#state{connection = Connection, channel = Channel, channel_ref = Ref}.
