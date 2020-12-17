%%% -------------------------------------------------------------------
%%% Author  : bokner
%%% Description : Message archive management (XEP-0313) for 1-to-1
%%%
%%% Created : Oct 12, 2012
%%% -------------------------------------------------------------------
-module(mod_11_timeline).

-behaviour(gen_server).
-behaviour(gen_mod).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("ejabberd.hrl").
-include("jlib.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% IQ handlers
-export([process_mam_iq/3]).

%% Message hook
-export([log_packet_out/1]).
%-export([filter_packet/1]).

%% For development only
-compile(export_all).

%% Internal state
-record(state, {host}).

%% Macros
-define(PROCNAME, ejabberd_mod_msg_archive).
-define(PROC(Server), gen_mod:get_module_proc(Server, ?PROCNAME)).
-define(THREAD_ID(Fuser, Tuser), crypto:hash(sha, sorted_str(Fuser, Tuser))).

-define(ZERO_TIMESTAMP, 0). 
-define(INFINITE_TIMESTAMP, 100000000000000000000). %% 1.0E+20
-define(DEFAULT_PAGE_SIZE, 20). %% Default page size (limit) for RSM
%%-define(CONCATENATE_STR(JidStr1, JidStr2), <<JidStr1/binary, JidStr2/binary>>).
%%-define(THREADID_TIMELINE_KEY(ThreadId), <<"11_TIMELINE:", ThreadId/binary>>).
%% ====================================================================
%% External functions
%% ====================================================================
start_link(Host, Opts) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
  gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
  ChildSpec =
    {Proc,
     {?MODULE, start_link, [Host, Opts]},
     permanent,
     1000,
     worker,
     [?MODULE]},
  supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
  supervisor:terminate_child(ejabberd_sup, Proc),
  supervisor:delete_child(ejabberd_sup, Proc). 

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Host, _Opts]) ->
    ejabberd_redis:start_link([
            {worker_config, ejabberd_config:get_local_option({redis_opts, Host})}
            ]),
  %% Have the module to handle the MAM traffic
  gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MAM,
                                ?MODULE, process_mam_iq, parallel),
  gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM,
                                ?MODULE, process_mam_iq, parallel),
  %% The hook to store 1-to-1 messages
  %ejabberd_hooks:add(user_send_packet, Host, ?MODULE, log_packet_out, 75),
  ejabberd_hooks:add(filter_packet, global, ?MODULE, log_packet_out, 75),
  {ok, #state{host = Host}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({route, #jid{luser = _U, lserver = _S} = _From, _To,  _Packet}, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    %ejabberd_hooks:delete(user_send_packet, State#state.host, ?MODULE, log_packet_out, 75),    
        ejabberd_hooks:delete(filter_packet, global, ?MODULE, log_packet_out, 75),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
process_mam_iq(From, _To, #iq{type = get, sub_el = Query} = IQ) ->
  %%?INFO_MSG("MAM IQ ~p",[{From, To, IQ}]),
  {WithJid, Start, End, Limit, Direction} = mam:resolve_request_params(From, Query),
  Result = process_mam(From, WithJid, Start, End, Limit, Direction),
  result_iq(Result, IQ, fun({_Timestamp, {_Nick, Msg}}) ->
                             send_archive_message(From, Msg, IQ#iq.id)
            end);

process_mam_iq(From, To, IQ) ->
  ?INFO_MSG("Unexpected request to MAM ~p",[{From, To, IQ}]),
  ignore.




process_mam(JidFrom, JidTo, Start, End, Limit, Direction) ->
  get_page(?THREAD_ID(JidFrom#jid.luser, JidTo#jid.luser), Start, End, Limit, Direction).

result_iq(Result, IQ, SendResultsFun) ->
    case Result of
      {ok, Page} ->
        lists:foreach(SendResultsFun, Page),
        IQ#iq{type = result, sub_el = []};
      {error, _Reason} ->
        IQ#iq{type = error, sub_el = []} 
    end.

       
%% Intercept incoming/outgoing messages of type "chat" (no type defaults to "chat")
log_packet_out({From, To, #xmlel{name = <<"message">>} = Stanza} = Packet ) ->
       Service = jlib:make_jid(<<>>, From#jid.lserver, <<>>),
       %EditTag = xml:get_subtag(Stanza,<<"replace">>),
       %DeleteTag = xml:get_subtag(Stanza,<<"delete">>),
       %NoteTag = xml:get_subtag(Stanza,<<"note">>),
       ResPacket = 
       %case {EditTag,DeleteTag,NoteTag} of
       %{false,false,false} ->        
               %% TODO : Log Chat Message
               case xml:get_tag_attr_s(<<"type">>, Stanza) of
                    Type when Type == <<"chat">>; Type == <<>> ->
                        case {xml:get_subtag(Stanza, <<"body">>),
                                              xml:get_subtag(Stanza, <<"result">>), %% used for mam 
                                              xml:get_subtag(Stanza, <<"delay">>), %% used by mod_offline 
                                              is_archived(Stanza)} %% used by archieve process
                                            of 
                            {false,_,_,_} ->
                                Packet;
                            {_,false,false,false} ->
                                   BodyMsg = xml:get_subtag_cdata(Stanza, <<"body">>),
                                   %BTags = xml:get_subtag_cdata(Stanza, <<"tags">>),
                                   %Tags = get_tag(BTags),
                                   TimeStamp = calendar:now_to_universal_time(now()),
                                   MamTs = mam:ts(),
                                   {SDateTime, SZ} = jlib:timestamp_to_iso(TimeStamp, utc),
                                   BTimestamp = list_to_binary(SDateTime ++ SZ),
                                   ?INFO_MSG("antbuddy_dev: Log Message: ~p ~n",[TimeStamp]),
                                   spawn(fun() ->
                                    %TimeStamp = calendar:now_to_universal_time(now()),    
                                        TSPacket = xml:append_subtags(Stanza,
                                                [jlib:timestamp_to_xml(TimeStamp, utc, From, <<>>)]),      
                                        ?INFO_MSG("antbuddy_dev: Log Message: Ts: ~p ~n",[mam:ts()]),
										case {xml:get_subtag(Stanza, <<"file">>),
												xml:get_subtag(Stanza, <<"oembed">>),
												xml:get_subtag(Stanza, <<"expandbody">>)} of
											%% normal message and add user message
											{false, false, false} ->
												log_msg(normal, From#jid.luser, BodyMsg, To#jid.luser, MamTs);
											%% message with file
											{FileData, false, false} ->
												log_msg(message_file, From#jid.luser, BodyMsg, To#jid.luser, MamTs, FileData);
											%% message with oembed
											{false, OembedData, false} ->
												log_msg(message_oembed, From#jid.luser, BodyMsg, To#jid.luser, MamTs, OembedData);
											%% bot message
											{false, false, _BotData} ->
												ExpandBody = xml:get_subtag_cdata(Stanza, <<"expandbody">>),
												log_msg(message_bot, From#jid.luser, BodyMsg, To#jid.luser, MamTs, ExpandBody);
											_ -> 
                                                Packet
										end
                                        %log_msg(?THREAD_ID(From#jid.luser, To#jid.luser), MamTs, {From#jid.luser, TSPacket})
                                        %log_tags(From#jid.luser, To#jid.luser, Tags, MamTs,  BTimestamp),
                                        %log_to_elasticsearch(MamTs, From#jid.luser, To#jid.luser, BodyMsg, Tags, BTimestamp)
                                    end),
                                    NewStanza = create_new_pack(Stanza, BTimestamp),
                                    %%Send echo message to sender
                                    ejabberd_router:route(Service, From, NewStanza),
                                    {From, To, NewStanza};
                           _ -> Packet
                         end;
                _Other ->
                    %void
                    Packet
            end,
        % _ ->
        %       ?INFO_MSG("kabam_dev: No Log Edit/Delete/Note Event Message ~n",[]),
        %        %void
        %        Packet
        %end,
       ResPacket ;
 
log_packet_out(Packet) ->
        Packet.
    %void.

stb(String) ->
        case String of
          Val when is_list(Val) ->
              erlang:list_to_binary(Val);
          Val when is_integer(Val) ->
                 integer_to_binary(Val);
          _ ->
              String
        end.

is_archived(Packet) ->
  case catch xml:get_tag_attr_s(<<"timestamp">>, Packet) of
       <<>> ->
            false;
       _ ->
            true
  end.


create_new_pack(#xmlel{attrs = Attrs} = Packet, BTimestamp) ->
  NewAttrs = lists:append(Attrs, [{<<"timestamp">>, BTimestamp}]),
  Packet#xmlel{attrs = NewAttrs}.

%% store with bot message
log_msg(message_bot, From, BodyMsg, To, Ts, ExpandBody) ->
    Message_id = mod_mam_utils:generate_message_id(),
	Doc = [{body, BodyMsg}, {fromId, From}, {id, Message_id}, {receiveId, To}, {senderId, From}, {expandBody, ExpandBody}, {isModified, <<"false">>}, {subtype, <<"bot-gitlab">>}, {type, <<"chat">>},{time, Ts}, {<<"__v">>, 0} ],
    mod_mongo:insert(Doc);

%% store message with oembed
%log_msg(From, BodyMsg, To, Ts, Html, AuthorUrl, AuthorName, ThumbnailHeight, Type, ProviderName, Version, ThumbnailUrl, Url, ThumbnailWidth, MeanAlpha, Title, Description, ProviderUrl) ->
log_msg(message_oembed, From, BodyMsg, To, Ts, OembedData) ->
	Message_id = mod_mam_utils:generate_message_id(),
	OembedInfo = [{html,				xml:get_subtag_cdata(OembedData, <<"html">>)}, 
					{author_url,		xml:get_subtag_cdata(OembedData, <<"author_url">>)}, 
					{author_name,		xml:get_subtag_cdata(OembedData, <<"author_name">>)}, 
					{thumbnail_height,	xml:get_subtag_cdata(OembedData, <<"thumbnail_height">>)}, 
					{type,				xml:get_subtag_cdata(OembedData, <<"type">>)}, 
					{provider_name,		xml:get_subtag_cdata(OembedData, <<"provider_name">>)}, 
					{version,			xml:get_subtag_cdata(OembedData, <<"version">>)}, 
					{thumbnail_url,		xml:get_subtag_cdata(OembedData, <<"thumbnail_url">>)}, 
					{url,				xml:get_subtag_cdata(OembedData, <<"url">>)}, 
					{thumbnail_width,	xml:get_subtag_cdata(OembedData, <<"thumbnail_width">>)}, 
					{mean_alpha,		xml:get_subtag_cdata(OembedData, <<"mean_alpha">>)}, 
					{title,				xml:get_subtag_cdata(OembedData, <<"title">>)}, 
					{description,		xml:get_subtag_cdata(OembedData, <<"description">>)}, 
					{provider_url,		xml:get_subtag_cdata(OembedData, <<"provider_url">>)}],
	Doc = [{senderId, From}, {body, BodyMsg}, {oembed, OembedInfo }, {fromId, From}, {receiveId, To}, {id, Message_id}, {type, <<"chat">>}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
	mod_mongo:insert(Doc);

%% store message with file
%log_msg(From, BodyMsg, To, Ts, FileName, FileSize, FileUrl, MimeType, ThumbnailUrl, ThumbnailWidth, ThumbnailHeight) ->
log_msg(message_file, From, BodyMsg, To, Ts, FileData) ->
	Message_id = mod_mam_utils:generate_message_id(),
	FileInfo = [{thumbnailUrl,			xml:get_subtag_cdata(FileData, <<"thumbnailUrl">>)}, 
				{thumbnailWidth,		xml:get_subtag_cdata(FileData, <<"thumbnailWidth">>)}, 
				{thumbnailHeight,		xml:get_subtag_cdata(FileData, <<"thumbnailHeight">>)}, 
				{mimeType,				xml:get_subtag_cdata(FileData, <<"mimeType">>)}, 
				{fileUrl,				xml:get_subtag_cdata(FileData, <<"fileUrl">>)}, 
				{size,					xml:get_subtag_cdata(FileData, <<"size">>)}, 
				{name,					xml:get_subtag_cdata(FileData, <<"name">>)}],
	Doc = [{senderId, From}, {body, BodyMsg}, {file, FileInfo}, {fromId, From}, {receiveId, To}, {id, Message_id}, {type, <<"chat">>}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
	mod_mongo:insert(Doc).

%%store add message
log_msg(add_user, From, BodyMsg, To, Ts) ->
    Message_id = mod_mam_utils:generate_message_id(),
    Doc = [{senderId, From}, {body, BodyMsg}, {fromId, From}, {receiveId, To}, {id, Message_id}, {subtype, <<"welcome">>}, {type, <<"chat">>},{time, Ts}, {<<"__v">>, 0}],
    mod_mongo:insert(Doc);

%% store normal message
log_msg(normal, From, BodyMsg, To, Ts) ->
	Message_id = mod_mam_utils:generate_message_id(),
	Doc = [{senderId, From}, {body, BodyMsg}, {fromId, From}, {receiveId, To}, {id, Message_id}, {type, <<"chat">>}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
	mod_mongo:insert(Doc).

%% Trace to delete messages based on tag topics.
log_tags(Fuser, Tuser, TagList, MamTs, BDateTime) ->
    Field = jsx:encode([{<<"key">>, sorted_str(Fuser, Tuser)}, {<<"datetime">>, BDateTime}]),
    lists:foreach(fun(Tag) ->
        %%ExtTags = lists:subtract([Tag], TagList),
        %%ExtTags = [{<<"tags">>, lists:subtract([Tag], TagList)}, {<<"datetime">>, BDateTime}],
        ExtTags = get_extra_tags(lists:subtract(TagList, [Tag]), Field),
        ?INFO_MSG("kabam_dev: Extra Tags: ~p ~n",[ExtTags]),
        Value = jsx:encode([{<<"from">>, Fuser}, {<<"to">>, Tuser},
                {<<"type">>, <<"chat">>}, {<<"ts">>, MamTs}, {<<"exttags">>, ExtTags}]),
        ejabberd_redis:cmd(["HSET", Tag, Field, Value])
    end, TagList).

%log_to_elasticsearch(TimeId, Fuser,Tuser,BodyMsg, Tags, BTimestamp) ->
%    Participants = jsx:encode([Fuser, Tuser]),
%    SortedStr = sorted_str(Fuser, Tuser),
%    BTimeId = stb(TimeId),
%    ESId = << SortedStr/binary, BTimeId/binary >>,
%    mod_elasticsearch:index_doc_with_id(?ESINDEX, ?ESTYPE, ESId, [{<<"from">>,Fuser},{<<"to">>,Tuser},{<<"participants">>,Participants}, {<<"type">>,<<"chat">>}, {<<"body">>,stb(BodyMsg)},{<<"tags">>, jsx:encode(Tags)}, {<<"timestamp">>,BTimestamp}]).

%% Retrieval of the archive page specified by the timestamp range and the size.
%%
get_page(ThreadId, StartTs, EndTs, PageSize, Direction) ->
    case ejabberd_redis:cmd(page_cmd(ThreadId, StartTs, EndTs, PageSize, Direction)) of           
        {error, Reason} ->
            error_logger:info_msg("Error in get_page", [Reason]),
            {error, Reason};
      Results ->        
        {ok, lists:map(fun(R) -> binary_to_term(R) end, 
            case Direction of 
                forward ->
                    Results;
                backward ->
                    lists:reverse(Results)
            end
        )}
  end.

%% Create the output for the MAM message, and route it to the requesting Jid
send_archive_message(To, Packet, QueryID) ->
  MAMPacket = 
    #xmlel{
        name = <<"message">>,
        attrs = [],
        children =  
          [   
          #xmlel{
        name = <<"result">>,
        attrs = [{<<"queryid">>, QueryID},
                 {<<"xmlns">>, ?NS_MAM},
                 {<<"id">>, randoms:get_string()}],
        children = [mam:forwarded(Packet)]}
         ]},
  
  ejabberd_router:route(jlib:make_jid(<<>>, To#jid.lserver, <<>>), 
                        To, xml:replace_tag_attr(<<"type">>, <<"headline">>, MAMPacket)).

page_cmd(ThreadId, StartTs, EndTs, PageSize, Direction) ->
    StartTsBin = integer_to_binary(StartTs),
    EndTsBin = integer_to_binary(EndTs),    
    case Direction of
        forward ->
            ["ZRANGEBYSCORE", ?THREADID_TIMELINE_KEY(ThreadId), 
                              << <<"(">>/binary, StartTsBin/binary>>,  EndTsBin, "LIMIT", 0, PageSize];
        backward ->
            ["ZREVRANGEBYSCORE", ?THREADID_TIMELINE_KEY(ThreadId), 
                            << <<"(">>/binary, EndTsBin/binary>>, StartTsBin, "LIMIT", 0, PageSize]
    end.


sorted_str(JidStr1, JidStr2) when JidStr2 > JidStr1 ->
  << JidStr1/binary, JidStr2/binary >>;
sorted_str(JidStr1, JidStr2) ->
  << JidStr2/binary, JidStr1/binary >>.

get_tag(BTags) ->
    case BTags of 
    <<>> ->
        [];
    _ ->
        StrTags = binary_to_list(BTags),
        TagsList = string:tokens(StrTags, ";"),
        lists:map(fun(Tag) ->
               stb(Tag)
              end, TagsList)
    end.

get_extra_tags(TagList, Field) ->
    lists:map(fun(ExtTag) ->
        %%[{<<"tag">>, ExtTag}, {<<"field">>, Field}]
        [ExtTag, Field]
    end, TagList).
