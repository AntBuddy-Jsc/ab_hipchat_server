
-module(mod_mongo).
-behaviour(gen_server).
-behaviour(gen_mod).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2,stop/1, insert/1, fetch/1, fetch_last_n/3, delete/1, update/2, insert_call/1, fetch_call/1, fetch_last_call/2, update_call/2]).

-include("ejabberd.hrl").
-define(MONGO_DB, <<"mongodb">>).
-define(MONGO_COLL, <<"chatstore1">>).
-define(MONGO_CALL_COLL, <<"callhistory">>).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {pool_id, dbcollection}).

%% start/2
%% Add interception hooks
%% ====================================================================
start(Host, Opts) ->
	?INFO_MSG("Hosts for mod_mongo : ~p ~n",[Host]),
	ensure_started(emongo),

    EHost = proplists:get_value(primary_ip, Opts, localhost),
    Port = proplists:get_value(port, Opts, 27017),
    PoolSize = proplists:get_value(pool_size, Opts, 10),
    MongoDB = proplists:get_value(dbname, Opts, ?MONGO_DB),
    ?INFO_MSG("Mongo Connections Configuration : ~p ~p ~p ~p~n",[EHost, Port, PoolSize, MongoDB]),

    case emongo:add_pool(mpools, EHost, Port, MongoDB, PoolSize) of
            ok ->
                ok;
            Error ->
                ?ERROR_MSG("Unable to create mongodb connection holder : ~p~n",[Error]),
                ok
    end,
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []).

ensure_started(emongo) ->
	case application:start(emongo) of
		ok -> 
			ok;
		{error, {already_started, emongo}} ->
			?INFO_MSG("AB_dev: emongo started ~n", []),
			ok;
		_ -> ?INFO_MSG("AB_dev: Error~n", []), error
	end.
%% stop/1
%% Remove interception hooks
%% ====================================================================
stop(Host) ->
	ok.

%%% ?MONGO_CALL_COLL : callhistory collection
insert_call(Doc) ->
    try
        emongo:insert(mpools, ?MONGO_CALL_COLL, Doc)
    catch
        _Err:Reason ->
             {'EXIT',{Reason,erlang:get_stacktrace()}} 
    end. 
	%gen_server:call(?MODULE, {insert_call, Doc}).

fetch_call(Selector) ->
    case catch  emongo:find(mpools, ?MONGO_CALL_COLL, Selector) of
        [] ->
            [];
        [H|_] = Res when is_list(H) ->
            Res;
        _ ->
            []
    end. 
	%gen_server:call(?MODULE, {fetch_call, Selector}).

fetch_last_call(Selector, Projector) ->
    case catch emongo:find(mpools, ?MONGO_CALL_COLL, Selector, Projector) of
        [] ->
            [];
        [H|_] = Res when is_list(H) ->
            Res;
        _ ->
            []
    end.
	%gen_server:call(?MODULE, {fetch_last_call, Selector, Projector}).

update_call(Selector, NDoc) ->
    try 
        emongo:update(mpools, ?MONGO_CALL_COLL, Selector, NDoc)
    catch 
        _Err:Reason ->
            {'EXIT',{Reason,erlang:get_stacktrace()}}
    end.
	%gen_server:call(?MODULE, {update_call, Selector, NDoc}).


%%% ?MONGO_COLL :  chatstore1 collection 
insert(Doc) ->
	gen_server:call(?MODULE, {insert, Doc}).

delete(Selector) ->
	gen_server:call(?MODULE, {delete, Selector}).

fetch(Selector) ->
	gen_server:call(?MODULE, {fetch, Selector}).

fetch_last_n(Selector, Projector, N) ->
	gen_server:call(?MODULE, {fetch_last_n, Selector, Projector, N}).

update(Selector, NDoc) ->
	gen_server:call(?MODULE, {update, Selector, NDoc}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Opts]) ->
    DBCOLL = proplists:get_value(dbcollection, Opts, ?MONGO_COLL),
    {ok, #state{dbcollection = DBCOLL}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({insert, Doc}, _From, State) ->
    try
        emongo:insert(mpools, State#state.dbcollection, Doc),
		{noreply, State}
    catch
        _Err:Reason ->
            {stop, Reason, State}
    end;

handle_call({delete, Selector}, _From, State) ->
    try
        emongo:delete(mpools, State#state.dbcollection, Selector),
		{noreply, State}
    catch
        _Err:Reason ->
            %{'EXIT',{Reason,erlang:get_stacktrace()}}
            {stop, Reason, State}
    end;

handle_call({fetch, Selector}, _From, State) ->
    Result = case catch emongo:find(mpools, State#state.dbcollection, Selector) of
        [] ->
            [];
        [H|_] = Res when is_list(H) ->
            Res;
        _ ->
            []
    end,
	{reply, Result, State};

handle_call({fetch_last_n, Selector, Projector, N}, _From, State) ->
    Result = case catch emongo:find(mpools, State#state.dbcollection, Selector, Projector) of
        [] ->
            [];
        [H|_] = Res when is_list(H) ->
            Res;
        _ ->
            []
    end,
	{reply, Result, State};

handle_call({update, Selector, NDoc}, _From, State) ->
    try
         emongo:update(mpools, State#state.dbcollection, Selector, NDoc),
         {noreply, State}
    catch
        _Err:Reason ->
            %{'EXIT', {Reason,erlang:get_stacktrace()}}
			{stop, Reason, State}
    end;

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

stb(String) ->
        case String of
          Val when is_list(Val) ->
            list_to_binary(Val);
      Val when is_integer(Val) ->
         integer_to_binary(Val);
          _ ->
              String
        end.
