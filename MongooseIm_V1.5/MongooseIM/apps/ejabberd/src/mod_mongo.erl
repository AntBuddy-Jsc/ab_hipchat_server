
-module(mod_mongo).
-behaviour(gen_server).
-behaviour(gen_mod).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2,stop/1, 
		insert/1, 
		fetch/1, 
		fetch_last_n/2,
        fetch_last/1, 
		delete/1, 
		update/2, 
		insert_bot/1,
		delete_bot/1,
		update_bot/2
        , insert_msg/1
		, fetch_msg/1
		, fetch_last_n_msg/3
        , fetch_last_msg/1
        , fetch_company/1]).

-export([create_social_bot/1
		, update_social_bot/2
		, delete_social_bot/1
		, fetch_social_bot/1
		, fetch_all_bots/0]).

-export([create_beeiq_app/1
        , fetch_beeiq_app/1
        , update_beeiq_app/2
        , delete_beeiq_app/1]).

-export([last_msg_time/1, bool_diff_ts_last_msg/1, bool_diff_ts_last_msg/2]).

-include("mod_mongo.hrl").

-define(MONTH_IN_MICRO_SECOND, 30*24*3600*1000000).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {pool_id
				, chatcollection
				, dbbot
				, messages_collection
                , db_socical_bot}).

%% start/2
%% Add interception hooks
%% ====================================================================
start(Host, Opts) ->
	?DEBUG("Hosts for mod_mongo : ~p ~n",[Host]),
	ensure_started(emongo),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Opts], []),
    mod_emongo_pools:start_link(Host, Opts).

ensure_started(emongo) ->
	case application:start(emongo) of
		ok -> 
			ok;
		{error, {already_started, emongo}} ->
			ok;
		_ -> ?ERROR_MSG("AB_dev: Error~n", []), error
	end.

%% stop/1
%% Remove interception hooks
%% ====================================================================
stop(_Host) ->
	ok.


%%% ?MONGO_COLL :  chatstore1 collection 
insert(Doc) ->
	%gen_server:cast(?MODULE, {insert, Doc}).
    case ets:lookup(track_chat_collections, chatcollection) of
        [{_, Collection}] ->
            try
                ok = emongo:insert(mpools, Collection, Doc)
            catch
                _Err:Reason ->
                    ?CRITICAL_MSG("Insert MongoDB exception ~p ~p ~p ~n", [_Err, Reason, erlang:get_stacktrace()]),
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ -> ok
    end.

delete(Selector) ->
	%gen_server:cast(?MODULE, {delete, Selector}).
    case ets:lookup(track_chat_collections, chatcollection) of
        [{_, Collection}] ->
            try
                emongo:delete(mpools, Collection, Selector)
            catch
                _Err:Reason ->
                    ?CRITICAL_MSG("Delete MongoDB exception ~p ~p ~p ~n", [_Err, Reason, erlang:get_stacktrace()]),
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ -> ok
    end.

fetch(Selector) ->
	%gen_server:call(?MODULE, {fetch, Selector}).
    case ets:lookup(track_chat_collections, chatcollection) of
        [{_, Collection}] ->
            case catch emongo:find(mpools, Collection, Selector) of
                [] ->
                    [];
                [H|_] = Res when is_list(H) ->
                    Res;
                _ ->
                    []
            end;
        _ -> []
    end.

fetch_last(Selector) ->
    Optional = [{limit, 1}, {orderby, [{time, desc}]}],
    case ets:lookup(track_chat_collections, chatcollection) of
        [{_, Collection}] ->
            case catch emongo:find(mpools, 
                        Collection, Selector, Optional) of
                [] ->
                    [];
                [H|_] = _Res when is_list(H) ->
                    H;
                _ ->
                    []
            end;
        _ -> []
    end.

last_msg_time(Selector) ->
    TS =
    case fetch_last(Selector) of
        Res when Res /= [] ->
            proplists:get_value(<<"time">>, Res);
        _ -> os:timestamp()
    end,
    mod_mam_utils:now_to_microseconds(TS).

bool_diff_ts_last_msg(Selector) ->
    bool_diff_ts_last_msg(Selector, ?MONTH_IN_MICRO_SECOND).

bool_diff_ts_last_msg(Selector, TimeIntervalMicro) ->
    NowMicro = mod_mam_utils:now_to_microseconds(os:timestamp()),
    DiffTS = NowMicro - last_msg_time(Selector),
    DiffTS >= TimeIntervalMicro.

fetch_last_n(Selector, Optional) ->
	%gen_server:call(?MODULE, {fetch_last_n, Selector, Optional}).
    case ets:lookup(track_chat_collections, chatcollection) of
        [{_, Collection}] ->
            case catch emongo:find(mpools, 
                        Collection, Selector, Optional) of
                [] ->
                    [];
                [H|_] = Res when is_list(H) ->
                    Res;
                _ ->
                    []
            end;
        _ -> []
    end.

update(Selector, NDoc) ->
	%gen_server:cast(?MODULE, {update, Selector, NDoc}).
    case ets:lookup(track_chat_collections, chatcollection) of
        [{_, Collection}] ->
            try
                emongo:update(mpools, Collection, Selector, NDoc)
            catch
                _Err:Reason ->
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ -> ok
    end.

%%% ?MONGO_BOT_COLL: bothistory collection
insert_bot(Doc) ->
	%gen_server:cast(?MODULE, {insert_bot, Doc}).
    case ets:lookup(track_chat_collections, botcollection) of
        [{_, Collection}] ->
            try
                emongo:insert(mpools, Collection, Doc)
            catch
                _Err:Reason ->
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ -> ok
    end.    

delete_bot(Selector) ->
    %gen_server:cast(?MODULE, {delete_bot, Selector}).
    case ets:lookup(track_chat_collections, botcollection) of
        [{_, Collection}] ->
            try
                emongo:delete(mpools, Collection, Selector)
            catch
                _Err:Reason ->
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ -> ok
    end.

update_bot(Selector, NDoc) ->
    %gen_server:cast(?MODULE, {update_bot, Selector, NDoc}).
    case ets:lookup(track_chat_collections, botcollection) of
        [{_, Collection}] ->
            try
                 emongo:update(mpools, Collection, Selector, NDoc)
            catch
                _Err:Reason ->
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ -> ok
    end.

%% AB#43
%%% MONGO_COLL_READ_MSG
insert_msg(Doc) ->
    %emongo:insert(mpools, ?MONGO_DB_READ_MSG, Doc).
    case ets:lookup(track_chat_collections, messages_collection) of
        [{_, Collection}] ->
            try
                ok = emongo:insert(msg_pools, Collection, Doc)
            catch
                _Err:Reason ->
                    ?CRITICAL_MSG("Insert MongoDB exception ~p ~p ~p ~n", [_Err, Reason, erlang:get_stacktrace()]),
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ -> ok
    end.


fetch_msg(Selector) ->
    %%gen_server:call(?MODULE, {fetch_msg, Selector}).
    case ets:lookup(track_chat_collections, messages_collection) of
        [{_, Collection}] ->
        	case catch emongo:find(msg_pools,
                            Collection, Selector) of
                [] ->
                    [];
                [H|_] = Res when is_list(H) ->
                    Res;
                _Err ->
                    []
            end;
        _ -> []
    end.

fetch_last_msg(Selector) ->
    Projector = [{limit, 1}, {orderby, [{time, desc}]}],
    case ets:lookup(track_chat_collections, messages_collection) of
        [{_, Collection}] ->
            case catch emongo:find(msg_pools,
                            Collection, Selector, Projector) of
                [] ->
                    [];
                [H|_] = Res when is_list(H) ->
                    Res;
                _ ->
                    []
            end;
        _ -> []
    end.

fetch_last_n_msg(Selector, Projector, _N) ->
    %%gen_server:call(?MODULE, {fetch_last_n_msg, Selector, Projector, N}).
    case ets:lookup(track_chat_collections, messages_collection) of
        [{_, Collection}] ->
        	case catch emongo:find(msg_pools,
                            Collection, Selector, Projector) of
                [] ->
                    [];
                [H|_] = Res when is_list(H) ->
                    Res;
                _ ->
                    []
            end;
        _ -> []
    end.

%%% MONGO_DB_ORGANIZATIONS
fetch_company(Selector) ->
    case catch emongo:find(msg_pools,
                    ?MONGO_DB_ORGANIZATIONS, Selector) of
        [] ->
            [];
        [H|_] = Res when is_list(H) ->
            Res;
        _ ->
            []
    end.   


%%%=================================================
%%%BeeIQ 
create_beeiq_app(Doc) ->
    case ets:lookup(track_chat_collections, social_bot) of
        [{_, Collection}] ->
            try
                 ok = emongo:insert(mpools, Collection, Doc)
            catch
                _Err:Reason ->
                    ?CRITICAL_MSG("create_beeiq_app MongoDB exception ~p ~p ~p ~n", [_Err, Reason, erlang:get_stacktrace()]),
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ ->
            ok
    end.

fetch_beeiq_app(Selector) ->
    case ets:lookup(track_chat_collections, social_bot) of
        [{_, Collection}] ->
            case catch emongo:find(mpools, Collection, Selector) of
                [] ->
                    [];
                [H|_] = Res when is_list(H) ->
                    Res;
                _ ->
                    []
            end;
        _ ->
            []
    end.

update_beeiq_app(Selector, Doc) ->
    %gen_server:call(?MODULE, {update_social_bot, Selector, Doc}).
    case ets:lookup(track_chat_collections, social_bot) of
        [{_, Collection}] ->
            try
                 emongo:update(mpools, Collection, Selector, Doc)
            catch
                _Err:Reason ->
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ -> ok
    end. 

delete_beeiq_app(Selector) ->
    %gen_server:call(?MODULE, {delete_social_bot, Selector}).
    case ets:lookup(track_chat_collections, social_bot) of
        [{_, Collection}] ->
            try
                emongo:delete(mpools, Collection, Selector)
            catch
                _Err:Reason ->
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ -> ok
    end.   


%%%=================================================
%%% CRUD of social bot
%%%=================================================
create_social_bot(Doc) ->
    %gen_server:call(?MODULE, {create_social_bot, Doc}).
    case ets:lookup(track_chat_collections, social_bot) of
        [{_, Collection}] ->
            try
                 ok = emongo:insert(mpools, Collection, Doc)
            catch
                _Err:Reason ->
                    ?CRITICAL_MSG("create_social_bot MongoDB exception ~p ~p ~p ~n", [_Err, Reason, erlang:get_stacktrace()]),
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ ->
            ok
    end.

update_social_bot(Selector, Doc) ->
    %gen_server:call(?MODULE, {update_social_bot, Selector, Doc}).
    case ets:lookup(track_chat_collections, social_bot) of
        [{_, Collection}] ->
            try
                 emongo:update(mpools, Collection, Selector, Doc)
            catch
                _Err:Reason ->
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ -> ok
    end.    

delete_social_bot(Selector) ->
    %gen_server:call(?MODULE, {delete_social_bot, Selector}).
    case ets:lookup(track_chat_collections, social_bot) of
        [{_, Collection}] ->
            try
                emongo:delete(mpools, Collection, Selector)
            catch
                _Err:Reason ->
                    {'EXIT',{Reason,erlang:get_stacktrace()}}
            end;
        _ -> ok
    end.

fetch_social_bot(Selector) ->
    case ets:lookup(track_chat_collections, social_bot) of
        [{_, Collection}] ->
            case catch emongo:find(mpools, Collection, Selector) of
                [] ->
                    [];
                [H|_] = Res when is_list(H) ->
                    Res;
                _ ->
                    []
            end;
        _ ->
            []
    end.

fetch_all_bots() ->
    case ets:lookup(track_chat_collections, social_bot) of
        [{_, Collection}] ->
            case catch emongo:find_all(mpools, Collection) of
                [] ->
                    [];
                [H|_] = Res when is_list(H) ->
                    Res;
                _ ->
                    []
            end;
        _ -> []
    end.

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
    DBCOLL = proplists:get_value(chatcollection, Opts, ?MONGO_COLL),
	DBBOT = proplists:get_value(botcollection, Opts, ?MONGO_BOT_COLL),
	DB_COLL_MSG = proplists:get_value(messages_collection, Opts, ?MONGO_DB_READ_MSG),
    %% store social bot
    DB_SOCIAL_BOT = proplists:get_value(social_bot, Opts, ?MONGO_DB_SOCIAL_BOT),
    %% add index to search
    create_track_chat_collections(DBCOLL, DBBOT, DB_COLL_MSG, DB_SOCIAL_BOT),
    catch emongo:ensure_index(mpools, DBCOLL, [{"fromKey", 1}, {"time", 1}]),
	catch emongo:ensure_index(mpools, DB_SOCIAL_BOT, [{"bot_app_id", 1}]),
    {ok, #state{chatcollection = DBCOLL
				, dbbot = DBBOT
				, messages_collection = DB_COLL_MSG
                , db_socical_bot = DB_SOCIAL_BOT}}.


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
% handle_call({insert, Doc}, _From, State) ->
%     try
%         emongo:insert(mpools, State#state.chatcollection, Doc),
% 		{reply, ok, State}
%     catch
%         _Err:Reason ->
%             {stop, Reason, State}
%     end;
% handle_call({delete, Selector}, _From, State) ->
%     try
%         emongo:delete(mpools, State#state.chatcollection, Selector),
% 		{reply, ok, State}
%     catch
%         _Err:Reason ->
%             %{'EXIT',{Reason,erlang:get_stacktrace()}}
%             {stop, Reason, State}
%     end;
% handle_call({fetch, Selector}, _From, State) ->
%     Result = case catch emongo:find(mpools, State#state.chatcollection, Selector) of
%         [] ->
%             [];
%         [H|_] = Res when is_list(H) ->
%             Res;
%         _ ->
%             []
%     end,
% 	{reply, Result, State};
% handle_call({fetch_last_n, Selector, Optional}, _From, State) ->
%     Result = case catch emongo:find(mpools, 
% 				State#state.chatcollection, Selector, Optional) of
%         [] ->
%             [];
%         [H|_] = Res when is_list(H) ->
%             Res;
%         _ ->
%             []
%     end,
% 	{reply, Result, State};
% handle_call({update, Selector, NDoc}, _From, State) ->
%     try
%          emongo:update(mpools, State#state.chatcollection, Selector, NDoc),
%          {reply, ok, State}
%     catch
%         _Err:Reason ->
%             %{'EXIT', {Reason,erlang:get_stacktrace()}}
% 			{stop, Reason, State}
%     end;
% handle_call({insert_bot, Doc}, _From, State) ->
%     try
%         emongo:insert(mpools, State#state.dbbot, Doc),
%         {reply, ok, State}
%     catch
%         _Err:Reason ->
%             {stop, Reason, State}
%     end;
% handle_call({delete_bot, Selector}, _From, State) ->
%     try
%         emongo:delete(mpools, State#state.dbbot, Selector),
%         {reply, ok, State}
%     catch
%         _Err:Reason ->
%             {stop, Reason, State}
%     end;
% handle_call({update_bot, Selector, NDoc}, _From, State) ->
%     try
%          emongo:update(mpools, State#state.dbbot, Selector, NDoc),
%          {reply, ok, State}
%     catch
%         _Err:Reason ->
%             {stop, Reason, State}
%     end;

%% AB#43
% handle_call({fetch_msg, Selector}, _From, State) ->
%     Result = case catch emongo:find(msg_pools, 
% 					State#state.messages_collection, Selector) of
%         [] ->
%             [];
%         [H|_] = Res when is_list(H) ->
%             Res;
%         _Err ->
%             []
%     end,
%     {reply, Result, State};
% handle_call({fetch_last_n_msg, Selector, Projector, _N}, _From, State) ->
%     Result = case catch emongo:find(msg_pools, 
% 					State#state.messages_collection, Selector, Projector) of
%         [] ->
%             [];
%         [H|_] = Res when is_list(H) ->
%             Res;
%         _ ->
%             []
%     end,
%     {reply, Result, State};
% handle_call({update_social_bot, Selector, NDoc}, _From, State) ->
%     try
%          Reply = emongo:update(mpools, State#state.db_socical_bot, Selector, NDoc),
%          {reply, Reply, State}
%     catch
%         _Err:Reason ->
%             {stop, Reason, State}
%     end;
% handle_call({create_social_bot, NDoc}, _From, State) ->
%     try
%          Reply = emongo:insert(mpools, State#state.db_socical_bot, NDoc),
%          {reply, Reply, State}
%     catch
%         _Err:Reason ->
%             {stop, Reason, State}
%     end;
% handle_call({delete_social_bot, Selector},_From, State) ->
%     try
%         Reply = emongo:delete(mpools, State#state.db_socical_bot, Selector),
%         {reply, Reply, State}
%     catch
%         _Err:Reason ->
%             {stop, Reason, State}
%     end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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
% handle_cast({insert, Doc}, State) ->
%     try
%         emongo:insert(mpools, State#state.chatcollection, Doc)
%     catch
%         _Err:Reason ->
%             {'EXIT',{Reason,erlang:get_stacktrace()}}
%     end,
% 	{noreply, State};
% handle_cast({update, Doc}, State) ->
%     try
%         emongo:update(mpools, State#state.chatcollection, Doc)
%     catch
%         _Err:Reason ->
%             {'EXIT',{Reason,erlang:get_stacktrace()}}
%     end,
%     {noreply, State};
% handle_cast({delete, Doc}, State) ->
%     try
%         emongo:delete(mpools, State#state.chatcollection, Doc)
%     catch
%         _Err:Reason ->
%             {'EXIT',{Reason,erlang:get_stacktrace()}}
%     end,
%     {noreply, State};
% handle_cast({insert_bot, Doc}, State) ->
%     try
%         emongo:insert(mpools, State#state.dbbot, Doc)
%     catch
%         _Err:Reason ->
%             {'EXIT',{Reason,erlang:get_stacktrace()}}
%     end,
% 	{noreply, State};
% handle_cast({delete_bot, Selector}, State) ->
%     try
%         emongo:delete(mpools, State#state.dbbot, Selector)
%     catch
%         _Err:Reason ->
%             {'EXIT',{Reason,erlang:get_stacktrace()}}
%     end,
% 	{noreply, State};
% handle_cast({update_bot, Selector, NDoc}, State) ->
%     try
%          emongo:update(mpools, State#state.dbbot, Selector, NDoc)
%     catch
%         _Err:Reason ->
%             {'EXIT',{Reason,erlang:get_stacktrace()}}
%     end,
% 	{noreply, State};
% handle_cast({create_social_bot, NDoc}, State) ->
%     try
%          emongo:insert(mpools, State#state.db_socical_bot, NDoc)
%     catch
%         _Err:Reason ->
%             {'EXIT',{Reason,erlang:get_stacktrace()}}
%     end,
%     {noreply, State};
% handle_cast({delete_social_bot, Selector}, State) ->
%     try
%         emongo:delete(mpools, State#state.db_socical_bot, Selector)
%     catch
%         _Err:Reason ->
%             {'EXIT',{Reason,erlang:get_stacktrace()}}
%     end,
%     {noreply, State};
% handle_cast({update_social_bot, Selector, NDoc}, State) ->
%     try
%          emongo:update(mpools, State#state.db_socical_bot, Selector, NDoc)
%     catch
%         _Err:Reason ->
%             {'EXIT',{Reason,erlang:get_stacktrace()}}
%     end,
%     {noreply, State};
handle_cast(_Msg, State) ->
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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
create_track_chat_collections(DBCOLL, DBBOT, DB_COLL_MSG, DB_SOCIAL_BOT) ->
    catch ets:new(track_chat_collections, [public, named_table, {write_concurrency, true}, {read_concurrency, true}]),
    ets:insert(track_chat_collections, {chatcollection, DBCOLL}),
    ets:insert(track_chat_collections, {botcollection, DBBOT}),
    ets:insert(track_chat_collections, {messages_collection, DB_COLL_MSG}),
    ets:insert(track_chat_collections, {social_bot, DB_SOCIAL_BOT}).

stb(String) ->
        case String of
          Val when is_list(Val) ->
            list_to_binary(Val);
      Val when is_integer(Val) ->
         integer_to_binary(Val);
          _ ->
              String
        end.
