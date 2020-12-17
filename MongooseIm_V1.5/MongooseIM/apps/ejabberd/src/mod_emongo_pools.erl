-module(mod_emongo_pools).

-include("mod_mongo.hrl").

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([start_link/2
        , add_pool/7       
        , get_pool/2]).

%% DEBUG 

-export([emongo_auth/3,
		getnonce/2]).

-define(MAX_TIMEOUT, 5000).
-define(PROCNAME, antbuddy_emongo_pools).
-define(MPOOLS, mpools).
-define(MSG_POOLS, msg_pools).
-record(state, {pool_size 
                , abhost
                , port
                , db_antbuddy
                , db_ab_user
				, db_ab_pass
                , webhost
                , db_web_user
				, db_web_pass
                , db_web}).

start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),    
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

init([Host, Opts]) ->
    catch ets:new(emongo_pool_connections, [named_table, set, public]),
    self() ! {init, Host, Opts},
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({init, _Host, Opts}, State) ->
    %% mongodb for antbuddy chat
    EHost = proplists:get_value(primary_ip, Opts, localhost),
    Port = proplists:get_value(port, Opts, 27017),

    PrimUser = proplists:get_value(pri_username, Opts, ""),
	PrimPass = proplists:get_value(pri_pass, Opts, ""),

    PoolSize = proplists:get_value(pool_size, Opts, 10),
    MongoDB = proplists:get_value(dbname, Opts, ?MONGO_DB),
    %% AB#43
    %% mongodb for kite chat
    WebHost = proplists:get_value(webserver_ip, Opts, localhost),
    MongoDB_MSG = proplists:get_value(dbname_message, Opts, ?MONGO_DB),
  	
  	WebUser = proplists:get_value(web_user, Opts, PrimUser),
	WebPass = proplists:get_value(web_pass, Opts, PrimPass),

    add_pool(?MPOOLS, EHost, Port, MongoDB, PoolSize, PrimUser, PrimPass),
    add_pool(?MSG_POOLS, WebHost, Port, MongoDB_MSG, PoolSize, WebUser, WebPass),
 
    {noreply, State#state{pool_size = PoolSize
                        , abhost = EHost
                        , port = Port
                        , db_antbuddy = MongoDB
                        , db_ab_user = PrimUser
						, db_ab_pass = PrimPass
                        , webhost = WebHost
                        , db_web_user = WebUser
						, db_web_pass = WebPass
                        , db_web = MongoDB_MSG}};

handle_info({'add_pool', PoolId, Host, Port, DatabaseName, PoolSize, Username, Pass}, State) ->
    add_pool(PoolId, Host, Port, DatabaseName, PoolSize, Username, Pass),    
    {noreply, State};

handle_info({'monitor_new_conenction', Pid, PoolId}, State) ->
    Ref = erlang:monitor('process', Pid),
    ets:insert(?EMONGO_CONNECTION_TABLE, {Pid, PoolId, Ref}), 
    {noreply, State};

% handle_info({'DOWN', _Reference, 'process', Pid, _Reason}, State) ->
%     ?INFO_MSG("AB#36: Disconnect mongodb ~p~n", [Pid]),
%     [{_, PoolId, Ref}] = ets:lookup(?EMONGO_CONNECTION_TABLE, Pid),
%     erlang:demonitor(Ref, ['flush']),
%     ets:delete(?EMONGO_CONNECTION_TABLE, Pid),
%     {noreply, reconnect(State, PoolId), 'hibernate'};

handle_info({'reconnect', PoolId}, State) ->
    {noreply, reconnect(State, PoolId), 'hibernate'};

handle_info(_Info, State) ->
	?INFO_MSG("_Info: ~p~n", [_Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

terminate(_Reason, #state{}) ->
	ets:delete(?EMONGO_CONNECTION_TABLE),
	ok.

add_pool(_PoolId, undefined, _Port, undefined, _PoolSize, _Username, _Pass) ->
	nothing_to_add_pool;
add_pool(PoolId, Host, Port, DatabaseName, PoolSize, Username, Pass) ->
	?INFO_MSG("#37: Open connection with PoolId ~p~n", [PoolId]),
	case get_pool(PoolId, emongo:pools()) of
		{{error, _}, _} ->
				erlang:send_after(?MAX_TIMEOUT, self(),
					{'add_pool', PoolId, Host, Port, DatabaseName, PoolSize, Username, Pass});
		undefined ->	
			case emongo:add_pool(PoolId, Host, Port, DatabaseName, PoolSize, Username, Pass) of
					ok ->
						case monitor_new_conenction(PoolId) of
							fail ->
								?ERROR_MSG("Unable to create mongodb connection holder~n",[]),
								erlang:send_after(?MAX_TIMEOUT, self(),
										{'add_pool', PoolId, Host, Port, DatabaseName, PoolSize, Username, Pass});
							ok -> 
								% auth(PoolId, Username, Pass),
								ok
						end;
					Error ->
						?ERROR_MSG("Unable to create mongodb connection holder : ~p~n",[Error]),
						erlang:send_after(?MAX_TIMEOUT, self(), 
										{'add_pool', PoolId, Host, Port, DatabaseName, PoolSize, Username, Pass})
			end;
		_ ->
			pool_exist
    end.

auth(PoolId, Username, Pass) ->
	if Username /= "" ->
		?INFO_MSG("#82: PoolId: ~p; Username: ~p",[PoolId, Username]),
		try emongo_auth(PoolId, Username, Pass) of 
			Res -> 
				?INFO_MSG("#37: Successfully open connection with PoolId ~p~n", [PoolId]),
				Res 
		catch 
			FailMsg ->
				handle_auth_throw(FailMsg, PoolId, Username, Pass)
		end;
	true -> 
		ok 
	end. 


%% TODO:  emongo:auth function bug so we override emongo:auth by this function 
emongo_auth(PoolId, Username, Pass) ->
	Pools = emongo:pools(),
    {Pool, _} = get_pool(PoolId, Pools),
    PoolPids = queue:to_list(Pool#pool.conn_pids),
    
    F = fun(Pid) ->
        case getnonce(Pid, Pool) of
            error ->
            	?ERROR_MSG("#82: fail getnonce: Pid ~p",[Pid]),
                throw(getnonce);
            Nonce ->
                do_auth(Nonce, Pid, Pool, Username, Pass)
        end
    end,
    lists:foreach(F, PoolPids).

do_auth(Nonce, Pid, Pool, User, Pass) ->
    Hash = emongo:dec2hex(erlang:md5(User ++ ":mongo:" ++ Pass)),
    Digest = emongo:dec2hex(erlang:md5(binary_to_list(Nonce) ++ User ++ binary_to_list(Hash))),
    Query = #emo_query{q=[{<<"authenticate">>, 1}, {<<"user">>, User}, {<<"nonce">>, Nonce}, {<<"key">>, Digest}], limit=1},
    Packet = emongo_packet:do_query(Pool#pool.database, "$cmd", Pool#pool.req_id, Query),
    Resp = emongo_conn:send_recv(Pid, Pool#pool.req_id, Packet, ?TIMEOUT),
    case lists:nth(1, Resp#response.documents) of 
	Docs when is_list(Docs) -> 
	    case lists:keyfind(<<"ok">>, 1, Docs) of
	        {<<"ok">>, 1.0} ->
	        	?INFO_MSG("#82: authenticated mongo: Pid: ~p",[Pid]),
	            {ok, authenticated};
	        _ ->
	        	?ERROR_MSG("#82: fail authenticated mongo: Pid ~p; Docs Error : ~p",[Pid, Docs]),
	            case lists:keyfind(<<"errmsg">>, 1, Docs) of
	                false ->
	                    throw(no_error_message);
	                {<<"errmsg">>, Error} ->
	                    throw(Error)
	            end
	    end ;
	_ ->
		?ERROR_MSG("#82: fail authenticated mongo: Pid ~p; Resp Error: ~p",[Pid, Resp]),
		throw(no_error_message)
    end.


getnonce(Pid, Pool) ->
    Query1 = #emo_query{q=[{<<"getnonce">>, 1}], limit=1},
    Packet = emongo_packet:do_query(Pool#pool.database, "$cmd", Pool#pool.req_id, Query1),
	Resp1 = emongo_conn:send_recv(Pid, Pool#pool.req_id, Packet, ?TIMEOUT),
	case lists:keyfind(<<"nonce">>, 1, lists:nth(1, Resp1#response.documents)) of
	    false ->
	        error;
	    {<<"nonce">>, Nonce} ->
	        Nonce
	end.

handle_auth_throw(no_error_message, _PoolId, _Username, _Pass) ->
	ok;
handle_auth_throw(FailMsg, _PoolId, _Username, _Pass) ->
	?ERROR_MSG("FailMsg: ~p",[FailMsg]).


get_pool(PoolId, Pools) ->
    get_pool(PoolId, Pools, []).
    
get_pool(_, [], _) ->
    undefined;
        
get_pool(PoolId, [{PoolId, Pool}|Tail], Others) ->
    {Pool, lists:append(Tail, Others)};
    
get_pool(PoolId, [Pool|Tail], Others) ->
    get_pool(PoolId, Tail, [Pool|Others]).

monitor_new_conenction(PoolId) ->
    case get_pool(PoolId, emongo:pools()) of
		{{error, _}, _} ->
				emongo:remove_pool(PoolId),
				fail;
		{Pool, _} ->
			case catch queue:to_list(Pool#pool.conn_pids) of
				{'EXIT', _} -> fail;
				Pids ->
					lists:foreach(fun(Pid) ->
						self() ! {monitor_new_conenction, Pid, PoolId}
					end, Pids), ok
			end
	end.

reconnect(#state{port=Port, pool_size=PoolSize}=State, PoolId) ->
    case get_pool(PoolId, emongo:pools()) of
        undefined ->
			%% get host and database name of poolid
			{Host, DatabaseName, Username, Pass} = case PoolId of
					?MPOOLS ->
						{State#state.abhost, State#state.db_antbuddy, State#state.db_ab_user, State#state.db_ab_pass};
					?MSG_POOLS ->
						{State#state.webhost, State#state.db_web, State#state.db_web_user, State#state.db_web_pass};
					_ ->
						{undefined, undefined, undefined, undefined}
			end,
			%% remove all record in ets of this poolid
			Connections = ets:match_object(?EMONGO_CONNECTION_TABLE, {'_', PoolId, '_'}),
			lists:foreach(fun({Pid, _, Ref}) -> 
				    erlang:demonitor(Ref, ['flush']),
				    ets:delete(?EMONGO_CONNECTION_TABLE, Pid)
			end, Connections),			
            add_pool(PoolId, Host, Port, DatabaseName, PoolSize, Username, Pass);
		{{error, _}, _} = R->
			?INFO_MSG("#37: Still reconnect with mongodb: ~p~n", [R]),
			erlang:send_after(?MAX_TIMEOUT, self(), {'reconnect', PoolId});
		{{pool,?MPOOLS,_,_,_,_,_,_},[]} ->
			erlang:send_after(?MAX_TIMEOUT, self(), {'reconnect', ?MSG_POOLS});
		{{pool,?MSG_POOLS,_,_,_,_,_,_},[]} ->
			erlang:send_after(?MAX_TIMEOUT, self(), {'reconnect', ?MPOOLS});
        Other ->
        	?INFO_MSG("reconnect: Other: ~p~n", [Other]),
            ok
    end,
	State;
reconnect(_, PoolId) ->
	erlang:send_after(?MAX_TIMEOUT, self(), {'reconnect', PoolId}).
