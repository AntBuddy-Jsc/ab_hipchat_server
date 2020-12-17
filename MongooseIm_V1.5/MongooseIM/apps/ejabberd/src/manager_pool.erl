-module(manager_pool).

-export([start_link/3
		, start_link/4
		, stop_pool/2
		, winfo/3
		, wcast/3
		, wcall/3
		, wcast/4
        , wcall/4
		, report_overrun/1
		, statistic/2
		, get_worker/2
    	, ensure_started/1
		]).

-include("ejabberd.hrl").

ensure_started(amqp_client) ->
	case application:start(amqp_client) of
	        ok ->
	            ok;
	        {error, {already_started, amqp_client}} ->
	            ok;
	        {error, {not_started, rabbit_common}} ->
	            case application:start(rabbit_common) of
	                ok ->
	                    application:start(amqp_client);
	                {error, {already_started, rabbit_common}} ->
	                    ok;
	                _ ->
	                    ?ERROR_MSG("AB#27: Error~n", []),
	                    error
	            end;
	        _ ->
	            ?ERROR_MSG("AB#27: Error~n", []), error
	end;
ensure_started(App) ->
  case application:start(App) of
    ok -> ok;
    _ -> error
  end.

start_link(Module, Host, Opts) ->
	Poolsize  = proplists:get_value(pool_size, Opts, 50),
	OverRun = proplists:get_value(overrun_warning, Opts, 5000),
	WPoolOptions    = [{overrun_warning, OverRun}
					, {overrun_handler, {manager_pool, report_overrun}}	
                    , {workers, Poolsize}
                    , {worker, {Module, [Host, Opts]}}
                    ],
	ChildMods = [ Module, ?MODULE ],
	ChildSpec = {supervisor_name(Module, Host),							%%ChildId, 
				{wpool, start_pool, [pool_name(Module, Host), WPoolOptions]},  %%StartFunc
				transient,												%%Restart
				2000,													%%Shutdown							
				supervisor,													%%Type 
				[wpool | ChildMods]},									%%Modules
	supervisor:start_child(mod_antbuddy_sup, ChildSpec).

start_link(Module, Host, Opts, {PoolName, Args}) ->
    Poolsize  = proplists:get_value(pool_channel, Opts, 50),
    OverRun = proplists:get_value(overrun_warning, Opts, 5000),
    WPoolOptions    = [{overrun_warning, OverRun}
                    , {overrun_handler, {manager_pool, report_overrun}}
                    , {workers, Poolsize}
                    , {worker, {Module, [Host, Opts, Args]}}
                    ],
	ChildMods = [ Module, ?MODULE ],
    ChildSpec = {supervisor_name(PoolName, Host),                         %%ChildId, 
                {wpool, start_pool, [pool_name(PoolName, Host), WPoolOptions]},  %%StartFunc
                transient,                                              %%Restart
                2000,                                                   %%Shutdown                          
                worker,                                                 %%Type 
                [wpool | ChildMods]},                                   %%Modules
    supervisor:start_child(mod_antbuddy_channel_sup, ChildSpec);

start_link(Supervisor, Module, Host, Opts) ->
	Poolsize = proplists:get_value(pool_size, Opts, 50),
	OverRun = proplists:get_value(overrun_warning, Opts, 5000),
	WPoolOptions = [{overrun_warning, OverRun},
					{overrun_handler, {Module, report_overrun}},
					{workers, Poolsize},
					{worker, {Module, [Host, Opts]}}],
	ChildMods = [ Module],
	ChildSpec = {supervisor_name(Module, Host),							%%ChildId, 
				{wpool, start_pool, [pool_name(Module, Host), WPoolOptions]},  %%StartFunc
				transient,												%%Restart
				2000,													%%Shutdown							
				supervisor,													%%Type 
				[wpool | ChildMods]},									%%Modules
	 supervisor:start_child(Supervisor, ChildSpec).


stop_pool(Module, Host) ->
	wpool:stop_pool(pool_name(Module, Host)).

-spec supervisor_name(atom(), list()) -> atom().
supervisor_name(Module, undefined) when is_atom(Module) ->
    list_to_atom(atom_to_list(Module)  ++ "_sup");
supervisor_name(Module, Host) when is_atom(Module), is_binary(Host) ->
    list_to_atom(atom_to_list(Module) ++ "_" ++ binary_to_list(Host) ++ "_sup").

-spec pool_name(atom(), binary())-> atom().
pool_name(Module, Host)->
    list_to_atom(atom_to_list(Module) ++ "_" ++ binary_to_list(Host) ++ "_pool").

wcast(Host, Module, Message)->
   wpool:cast(pool_name(Module, Host), Message).

wcall(Host, Module, Message) ->
	wpool:call(pool_name(Module, Host), Message).

wcast(Host, Module, Message, Strategy)->
   wpool:cast(pool_name(Module, Host), Message, Strategy).

wcall(Host, Module, Message, Strategy) ->
    wpool:call(pool_name(Module, Host), Message, Strategy).

winfo(Host, Module, Message) ->
	WorkerId = get_worker(Module, Host),
    WorkerId ! {worker, Message}.

get_worker(Host, Module) ->
	wpool:call(pool_name(Module, Host), {erlang, self, []}, best_worker).

statistic(Module, Host) ->
	Get = fun proplists:get_value/2,
	InitStats = wpool:stats(pool_name(Module, Host)),
	PoolPid = Get(supervisor, InitStats),
	Options = Get(options, InitStats),
	InitWorkers = Get(workers, InitStats),
	WorkerStatus = 
	[begin
	    WorkerStats = Get(I, InitWorkers),
	    MsgQueueLen = Get(message_queue_len, WorkerStats),
	    Memory = Get(memory, WorkerStats),
	    {status, WorkerStats, MsgQueueLen, Memory}
	end || I <- lists:seq(1, length(InitWorkers))],
	[PoolPid, Options, WorkerStatus].


-spec report_overrun(term()) -> ok.
report_overrun(Report) ->
  lager:error("~p", [Report]).
