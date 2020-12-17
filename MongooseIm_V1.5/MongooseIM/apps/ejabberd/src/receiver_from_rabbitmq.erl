-module(receiver_from_rabbitmq).
-behaviour(gen_mod).
-behaviour(gen_server).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_rabbitmq.hrl").

-export([start_link/2,
		create_consumer/1]).
-export([start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MOD_RABBITMQ, mod_rabbitmq).
-define(RABBITMQ_POOL, manager_pool).
-define(SUB_PROCESS, mod_rabbitmq_channel).
-record(state, {connection, pool_name}).

start(Host, Opts) ->
   ?MOD_RABBITMQ:ensure_started(),
   ?RABBITMQ_POOL:start_link(?MODULE, Host, Opts).

stop(_Host) ->
   ?RABBITMQ_POOL:stop(?MODULE).

start_link(Host, Opts) ->
	gen_server:start_link(?MODULE, [Host,Opts],[]).

init([Host,Opts]) ->
    %% Start a connection to the server
    Connection = case ?MOD_RABBITMQ:open_connection(Opts) of 
                    {ok, Conn} -> 
                      Conn;
                    undefined -> 
                      undefined 
                  end,
	%% create a pool channel for this connection.
    PoolName = list_to_atom( atom_to_list(?SUB_PROCESS) ++ integer_to_list(timestamp(os:timestamp())) ),
	?RABBITMQ_POOL:start_link(PoolName, Host, Opts, {?SUB_PROCESS, Connection}),
    {ok, #state{connection = Connection, pool_name = PoolName}}.


handle_call(stop,_From, State) ->
   {stop, normal, ok, State};

handle_call(_Event, _From, State) ->
	{reply, ok, State}.

handle_cast({create_consumer, RoomJID}, #state{pool_name = PoolName} = State) ->
	?RABBITMQ_POOL:wcast(PoolName, {create_consumer, RoomJID}, next_worker),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, #state{connection = Connection, pool_name = PoolName}) ->
	?ERROR_MSG("AB#27 Stop Receive pool ~p~n", [_Reason]),
	?RABBITMQ_POOL:stop(PoolName),
    ?MOD_RABBITMQ:close_connection(Connection).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({create_consumer, RoomJID}, #state{pool_name = PoolName} = State) ->
    ?RABBITMQ_POOL:wcast(PoolName, {create_consumer, RoomJID}, next_worker),
    {noreply, State};

handle_info({#'basic.consume_ok'{}}, State) ->
    ?INFO_MSG("AB#27: basic_consume_ok: Subscribe channel ~n",[]),
    {noreply, State};

handle_info({#'basic.cancel_ok'{}}, State) ->
    ?INFO_MSG("AB#27: basic_cancel: Cancel subscribe this channel ~n",[]),
    {noreply, State};

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = _BPayLoad}}, State) ->
	?INFO_MSG("AB#27: Receiving pool connection ~n", []),
	{noreply, State};

handle_info(Msg, State) ->
    ?INFO_MSG("Other Incoming Message ~p ",[ Msg ]),
    {noreply, State}.

create_consumer(RoomJID) ->
	?RABBITMQ_POOL:wcast(?MODULE, {create_consumer, RoomJID}, next_worker).

timestamp({Mega, Secs, Micro}) ->
    Mega * 1000000 * 1000000 + Secs * 1000000 + Micro.

