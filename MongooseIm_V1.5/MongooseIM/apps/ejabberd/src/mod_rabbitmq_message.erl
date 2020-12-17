-module(mod_rabbitmq_message).

-behaviour(gen_mod).
-behaviour(gen_server).

-include("mod_rabbitmq.hrl").

-record(state, {connection,
                connection_old, 
                connection_ref,
                pool_name, 
                host,
                opts
                }).

%% gen_mod 
-export([start/2, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([publish_message/2]).

-define(CHANNEL_POOL, manager_pool).
-define(SUB_PROCESS, mod_rabbitmq_message_channel).
-define(START_TIMEOUT, 200).
-define(MAX_TIMEOUT, 1000).

timestamp({Mega, Secs, Micro}) ->
    Mega * 1000000 * 1000000 + Secs * 1000000 + Micro.

start(Host, Opts) ->
  ?CHANNEL_POOL:ensure_started(amqp_client),

  create_producer(Opts),

  ets:new(?MESSAGE_CHANNEL_TABLE, [bag, named_table, public]),
  ?CHANNEL_POOL:start_link(?MODULE, Host, Opts).

stop(_Host) ->  
  ok.

publish_message(Host, Payload) -> 
  ?DEBUG("AB#65: publish push message to rabbitmq: ~p~n", [Payload]),
  ?CHANNEL_POOL:wcast(Host, ?MODULE, 
            {publish_message, Payload}, next_worker).

init([Host, Opts]) ->
    self() ! init,
    {ok, #state{host = Host,
                opts = Opts} }.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast({publish_message, BPayload}, 
        #state{pool_name = PoolName, host = Host} = State) ->
  ?CHANNEL_POOL:wcast(Host, PoolName, 
            {publish_message, BPayload}, next_worker),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(init, #state{host = Host, opts = Opts} = State) -> 
    %% Start a connection to the server
    case mod_rabbitmq:open_connection(Opts) of 
        undefined -> 
           ?ERROR_MSG("AB#65 Cannot connection to RabbitMQ server: ~p~n", [Opts]),
           {stop, connect_error, State};

        {ok, Connection} ->
            %% create a pool channel for this connection.
            PoolName = list_to_atom(atom_to_list(?SUB_PROCESS) ++ "_" ++ integer_to_list(timestamp(os:timestamp())) ),
            ?CHANNEL_POOL:start_link(?SUB_PROCESS, Host, Opts, {PoolName, Connection}),
            Ref = erlang:monitor('process', Connection),
            ?DEBUG("AB#65 Create workers pool: ~p, opts: ~p, connection: ~p~n", [PoolName, Opts, Connection]),
            {noreply, State#state{connection = Connection, 
                                  connection_ref = Ref, 
                                  pool_name = PoolName}} 
    end;
 

handle_info({publish_message, BPayload}, 
        #state{pool_name = PoolName, host = Host} = State) ->
    ?CHANNEL_POOL:wcast(Host, PoolName, 
            {publish_message, BPayload}, next_worker),
    {noreply, State};


%% AB#36
handle_info({'DOWN', _Reference, 'process', Pid, _Reason}, State) ->
  ?INFO_MSG("AB#36: Disconnect rabbitmq ~p~n", [Pid]),
  {noreply, disconnected(State), hibernate};
handle_info({'connect', Timeout}, State) ->
  {noreply, maybe_connect(State, Timeout), 'hibernate'};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(Reason, #state{connection = Connection}) ->
  ?DEBUG("AB#65: process terminate with reason: ~p~n", [Reason]),
  mod_rabbitmq:close_connection(Connection).

-spec next_timeout(pos_integer()) -> ?START_TIMEOUT..?MAX_TIMEOUT.
next_timeout(?MAX_TIMEOUT = Timeout) ->
    Timeout;
next_timeout(Timeout) when Timeout*2 > ?MAX_TIMEOUT ->
    ?MAX_TIMEOUT;
next_timeout(Timeout) when Timeout < ?START_TIMEOUT ->
    ?START_TIMEOUT;
next_timeout(Timeout) ->
    Timeout * 2.

disconnected(State) ->
  disconnected(State, ?START_TIMEOUT).
disconnected(#state{connection_ref = Ref} = State, Timeout)
  when is_reference(Ref) ->
    erlang:demonitor(Ref, ['flush']),
    disconnected(State#state{connection_ref = 'undefined'}, Timeout);
disconnected(#state{connection = Pid} = State, Timeout)
  when is_pid(Pid) ->
    case is_process_alive(Pid) of 
        true ->
          _ = (catch mod_rabbitmq:close_connection(Pid));
        _ -> 
          ok
    end,
  disconnected(State#state{connection = 'undefined', connection_old = Pid}, Timeout);
disconnected(State, Timeout) ->
  NextTimeout = next_timeout(Timeout),
  erlang:send_after(Timeout, self(), {'connect', NextTimeout}),
  State.

check_process_alive(Pid) ->
	Nodes = [node() | nodes()], 
	rpc:call(Nodes, erlang, is_process_alive, [Pid]).

%% Update channel pool
connected(#state{connection = Connection,
                  connection_old = Connection_Old} = State) ->
  Records = ets:lookup(?MESSAGE_CHANNEL_TABLE, Connection_Old),
  Workers = [Worker || {_Conn, Worker} <- Records],
  ?DEBUG("AB#65: state: ~p, workers: ~p~n", [State, Workers]), 
  lists:foreach(fun(Worker) -> Worker ! {update_channel, Connection} end, Workers),
  ets:delete(?MESSAGE_CHANNEL_TABLE, Connection_Old), 
  State#state{connection_old = 'undefined'}.

maybe_connect(#state{opts = Opts}=State, Timeout) ->
    case mod_rabbitmq:open_connection(Opts) of
        undefined ->
            ?DEBUG("AB#36: amqp connection failure, will retry",[]),
            disconnected(State, Timeout);
        {ok, Pid} ->
            ?INFO_MSG("AB#36: connected to rabbitmq Params: ~p~n", [Opts]),
            Ref = erlang:monitor('process', Pid),
            connected(State#state{connection=Pid, connection_ref=Ref});
        Res ->
          ?DEBUG("Res: ~p~n", [Res]),
          Res
    end.

create_producer(Opts) -> 
    case mod_rabbitmq:open_connection(Opts) of
        {ok, Conn} -> 
            Channel = mod_rabbitmq:open_channel(Conn),
            {QueueName, ExchangeName, RK} = {?QUEUE_AB_PUSH, ?EXCHANGE_AB_PUSH, ?ROUTING_KEY_AB_PUSH},
            Exchange = case mod_rabbitmq:exchange_declare(Channel, ExchangeName, <<"direct">>) of
                            error -> [];
                            _ -> ExchangeName
                       end,
            Queue = case mod_rabbitmq:queue_declare(Channel, QueueName) of
                        error -> [];
                        _ -> QueueName
                    end,
            case mod_rabbitmq:queue_bind(Channel, Queue, Exchange, RK)  of
                error -> 
                    ?ERROR_MSG("AB#65 Cannot create producer: Channel: ~p, Queue: ~p, Exchange: ~p, RK: ~p~n", [Channel, Queue, Exchange, RK]),
                    error;
                _ ->
                    ?INFO_MSG("AB#65 Create Exchange: ~p, Queue: ~p with Routing Key: ~p ... OK!~n", [ExchangeName, QueueName, RK]),
                    mod_rabbitmq:close_channel(Channel),
                    mod_rabbitmq:close_connection(Conn),
                    ok
            end;
        undefined -> 
            ?ERROR_MSG("AB#65 Cannot connect to RabbitMQ Server: ~p~n", [Opts]),
            error
    end.
