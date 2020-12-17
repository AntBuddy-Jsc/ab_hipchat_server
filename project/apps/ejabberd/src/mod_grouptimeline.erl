%%% -------------------------------------------------------------------
%%% Author  : bokner
%%% Description : Message archive management for groups (XEP-0313 variation)
%%%
%%% Created : May 09, 2014
%%% -------------------------------------------------------------------
-module(mod_grouptimeline).

-behaviour(gen_server).
-behaviour(gen_mod).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mam.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% IQ handlers
-export([process_mam_iq/3]).

%% Interface to mod_muc_room
-export([get_history/2]).
-export([room_packet_hook/4]).



%% For development only
-compile(export_all).

%% Internal state
-record(state, {host}).

%% Macros
-define(PROCNAME, ejabberd_mod_group_timeline).
-define(PROC(Server), gen_mod:get_module_proc(Server, ?PROCNAME)).


%%-define(GROUPID_TIMELINE_KEY(GroupId), <<"GROUP_TIMELINE:", GroupId/binary>>).

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
init([Host, Opts]) ->
    ConferenceHost = gen_mod:get_opt_host(Host, Opts, <<"conference.@HOST@">>),
    ejabberd_redis:start_link([
            {worker_config, ejabberd_config:get_local_option({redis_opts, Host})}
            ]),
    %% Have the module to handle the MAM traffic
    mod_muc_iq:register_iq_handler(ConferenceHost, ?NS_MAM,
                                  ?MODULE, process_mam_iq),
    %% The hook to store room messages
    %% Note: the hook is associated with conference host
    ejabberd_hooks:add(room_packet, ConferenceHost, ?MODULE, room_packet_hook, 75),
    ejabberd_hooks:add(filter_room_packet, ConferenceHost, ?MODULE,filter_room_packet, 90),
    {ok, #state{host = ConferenceHost}}.

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
terminate(_Reason, State) ->
    mod_muc_iq:unregister_iq_handler(State#state.host, ?NS_MAM),
    ejabberd_hooks:delete(room_packet, State#state.host, ?MODULE, room_packet_hook, 75),
    ejabberd_hooks:delete(filter_room_packet, State#state.host, ?MODULE, filter_room_packet, 90),
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
process_mam_iq(From, GroupJid, #iq{type = get, sub_el = Query} = IQ) ->
  {_WithJid, Start, End, Limit, Direction} = mam:resolve_request_params(From, Query),
  Result = process_mam(GroupJid#jid.luser, Start, End, Limit, Direction),
  result_iq(Result, IQ, 
            fun({Nick, Msg}) ->
                send_archive_message(From, GroupJid, Msg, Nick)
            end);

process_mam_iq(From, To, IQ) ->
  ?INFO_MSG("Unexpected request to MAM ~p",[{From, To, IQ}]),
  ignore.


process_mam(GroupId, Start, End, Limit, Direction) ->
    get_page(GroupId, Start, End, Limit, Direction).

result_iq(Result, IQ, SendResultsFun) ->
    case Result of
      {ok, Page} ->
        lists:foreach(SendResultsFun, Page),
        IQ#iq{type = result, sub_el = []};
      {error, _Reason} ->
        IQ#iq{type = error, sub_el = []} %%TODO: add more details to error handling
    end.

%% store with bot message
log_msg(message_bot, From, BodyMsg, RoomID, Ts, ExpandBody) ->
    Message_id = mod_mam_utils:generate_message_id(),
    Doc = [{body, BodyMsg}, {fromId, RoomID}, {id, Message_id}, {receiveId, <<"null">>}, {senderId, From}, {expandBody, ExpandBody}, {isModified, <<"false">>}, {subtype, <<"bot-gitlab">>}, {type, <<"groupchat">>},{time, Ts}, {<<"__v">>, 0} ],
    mod_mongo:insert(Doc);

%% store message with oembed
log_msg(message_oembed, From, BodyMsg, RoomID, Ts, OembedData) ->
    Message_id = mod_mam_utils:generate_message_id(),
    OembedInfo = [{html,                xml:get_subtag_cdata(OembedData, <<"html">>)}, 
                    {author_url,        xml:get_subtag_cdata(OembedData, <<"author_url">>)}, 
                    {author_name,       xml:get_subtag_cdata(OembedData, <<"author_name">>)}, 
                    {thumbnail_height,  xml:get_subtag_cdata(OembedData, <<"thumbnail_height">>)}, 
                    {type,              xml:get_subtag_cdata(OembedData, <<"type">>)}, 
                    {provider_name,     xml:get_subtag_cdata(OembedData, <<"provider_name">>)}, 
                    {version,           xml:get_subtag_cdata(OembedData, <<"version">>)}, 
                    {thumbnail_url,     xml:get_subtag_cdata(OembedData, <<"thumbnail_url">>)}, 
                    {url,               xml:get_subtag_cdata(OembedData, <<"url">>)}, 
                    {thumbnail_width,   xml:get_subtag_cdata(OembedData, <<"thumbnail_width">>)}, 
                    {mean_alpha,        xml:get_subtag_cdata(OembedData, <<"mean_alpha">>)}, 
                    {title,             xml:get_subtag_cdata(OembedData, <<"title">>)}, 
                    {description,       xml:get_subtag_cdata(OembedData, <<"description">>)}, 
                    {provider_url,      xml:get_subtag_cdata(OembedData, <<"provider_url">>)}],
    Doc = [{senderId, From}, {body, BodyMsg}, {oembed, OembedInfo }, {fromId, RoomID}, {receiveId, <<"null">>}, {id, Message_id}, {type, <<"groupchat">>}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
    mod_mongo:insert(Doc);

%% store message with file
log_msg(message_file, From, BodyMsg, RoomID, Ts, FileData) ->
    Message_id = mod_mam_utils:generate_message_id(),
    FileInfo = [{thumbnailUrl,          xml:get_subtag_cdata(FileData, <<"thumbnailUrl">>)}, 
                {thumbnailWidth,        xml:get_subtag_cdata(FileData, <<"thumbnailWidth">>)}, 
                {thumbnailHeight,       xml:get_subtag_cdata(FileData, <<"thumbnailHeight">>)}, 
                {mimeType,              xml:get_subtag_cdata(FileData, <<"mimeType">>)}, 
                {fileUrl,               xml:get_subtag_cdata(FileData, <<"fileUrl">>)}, 
                {size,                  xml:get_subtag_cdata(FileData, <<"size">>)}, 
                {name,                  xml:get_subtag_cdata(FileData, <<"name">>)}],
    Doc = [{senderId, From}, {body, BodyMsg}, {file, FileInfo}, {fromId, RoomID}, {receiveId, <<"null">>}, {id, Message_id}, {type, <<"groupchat">>}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
    mod_mongo:insert(Doc).

%%store add message
log_msg(add_user, From, BodyMsg, RoomID, Ts) ->
    Message_id = mod_mam_utils:generate_message_id(),
    Doc = [{senderId, From}, {body, BodyMsg}, {fromId, RoomID}, {receiveId, <<"null">>}, {id, Message_id}, {subtype, <<"welcome">>}, {type, <<"groupchat">>},{time, Ts}, {<<"__v">>, 0}],
    mod_mongo:insert(Doc);

%% store normal message
log_msg(normal, From, BodyMsg, RoomID, Ts) ->
    Message_id = mod_mam_utils:generate_message_id(),
    Doc = [{senderId, From}, {body, BodyMsg}, {fromId, RoomID}, {receiveId, <<"null">>}, {id, Message_id}, {type, <<"groupchat">>}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
    mod_mongo:insert(Doc).

%log_msg(GroupId, Ts, PacketData) ->
    %MsgRecord = term_to_binary(PacketData),
    %mod_mongo:insert([{group_id, ?GROUPID_TIMELINE_KEY(GroupId)},{timestamp, Ts}, {message_body, MsgRecord}]).
    %ejabberd_redis:cmd(["ZADD", ?GROUPID_TIMELINE_KEY(GroupId), Ts, MsgRecord]).

%% Trace to delete messages based on tag topics.
log_tags(FromNick, GroupId, TagList, MamTs, BDateTime) ->
        Field = jsx:encode([{<<"key">>, sorted_str(FromNick, GroupId)}, {<<"datetime">>, BDateTime}]),
        lists:foreach(fun(Tag) ->
                ExtTags = get_extra_tags(lists:subtract(TagList, [Tag]), Field),
                Value = jsx:encode([{<<"from">>, FromNick}, {<<"to">>, GroupId},{<<"type">>, <<"groupchat">>}, 
                                        {<<"ts">>, MamTs}, {<<"exttags">>, ExtTags}]),
                ejabberd_redis:cmd(["HSET", Tag, Field, Value])
        end, TagList).

%log_to_elasticsearch(TimeId, Fuser, RoomJid, BodyMsg, Tags, BTimestamp) ->
%     BTimeId = stb(TimeId),
%     GroupId = RoomJid#jid.luser,
%     ESId = << GroupId/binary, BTimeId/binary >>,   
%     Participants  = get_room_participants(RoomJid),
%     BParticipants = jsx:encode(Participants),
%     mod_elasticsearch:index_doc_with_id(?ESINDEX, ?ESTYPE, ESId, [{<<"from">>,Fuser},{<<"to">>, GroupId}, {<<"participants">>, BParticipants},{<<"type">>,<<"groupchat">>},{<<"body">>,stb(BodyMsg)},{<<"tags">>, jsx:encode(Tags)},{<<"timestamp">>,BTimestamp}]).
       

%% Interface to mod_muc_room
%% Get the last page of the room history
get_history(GroupJid, PageSize) ->
  {ok, LastPage} = get_page(GroupJid#jid.luser, ?ZERO_TIMESTAMP, ?INFINITE_TIMESTAMP, PageSize, backward),
  LastPage.

%% Retrieval of the archive page specified by the timestamp range and the size.
%%
get_page(GroupId, StartTs, EndTs, PageSize, Direction) ->
    case ejabberd_redis:cmd(page_cmd(GroupId, StartTs, EndTs, PageSize, Direction)) of        
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

%% Create the output for the archive message, and route it to the requesting Jid
send_archive_message(From, GroupJid, Msg, Nick) ->
    ejabberd_router:route(jlib:jid_replace_resource(GroupJid, Nick),
                           From, Msg).

page_cmd(GroupId, StartTs, EndTs, PageSize, Direction) ->
    StartTsBin = integer_to_binary(StartTs),
    EndTsBin = integer_to_binary(EndTs),    
    case Direction of
        forward ->
            ["ZRANGEBYSCORE", ?GROUPID_TIMELINE_KEY(GroupId), 
                              << <<"(">>/binary, StartTsBin/binary>>,  EndTsBin, "LIMIT", 0, PageSize];
        backward ->
            ["ZREVRANGEBYSCORE", ?GROUPID_TIMELINE_KEY(GroupId), 
                            << <<"(">>/binary, EndTsBin/binary>>, StartTsBin, "LIMIT", 0, PageSize]
    end.


room_packet_hook(FromNick, FromJID, RoomJid, Packet) ->
  %EditTag = xml:get_subtag(Packet,<<"replace">>),
  %DeleteTag = xml:get_subtag(Packet,<<"delete">>),
  %NoteTag = xml:get_subtag(Packet,<<"note">>),
  %case {EditTag,DeleteTag,NoteTag} of
  %      {false,false,false} ->
            TimeStamp = calendar:now_to_universal_time(now()), 
            {SDateTime, SZ} = jlib:timestamp_to_iso(TimeStamp, utc),
            BTimestamp = list_to_binary(SDateTime ++ SZ),
            BodyMsg = xml:get_subtag_cdata(Packet, <<"body">>),
            %BTags = xml:get_subtag_cdata(Packet, <<"tags">>),
            %Tags = get_tag(BTags),
            MamTs = mam:ts(),
            ?INFO_MSG("AntBuddy_dev: Log Message: Ts: ~p ~n",[MamTs]),
			case {xml:get_subtag(Packet, <<"file">>),
				xml:get_subtag(Packet, <<"oembed">>),
				xml:get_subtag(Packet, <<"expandbody">>)} of
				%% normal message and add user message
				{false, false, false} ->
					case xml:get_tag_attr_s(<<"subtype">>, Packet) of
						SubType when SubType == <<"welcome">> ->
							log_msg(add_user, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, MamTs);
						_ ->
							log_msg(normal, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, MamTs)
					end;
				%% message with file
				{FileData, false, false} ->
					log_msg(message_file, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, MamTs, FileData);
					%% message with oembed
					{false, OembedData, false} ->
						log_msg(message_oembed, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, MamTs, OembedData);
					%% bot message
					{false, false, _BotData} ->
						ExpandBody = xml:get_subtag_cdata(Packet, <<"expandbody">>),
						log_msg(message_bot, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, MamTs, ExpandBody);
				_ ->
					Packet
			end.
            %log_msg(RoomJid#jid.luser, MamTs, {FromNick, Packet});
            %log_tags(FromNick, RoomJid#jid.luser, Tags, MamTs,  BTimestamp),
            %log_to_elasticsearch(MamTs, FromNick, RoomJid, BodyMsg, Tags, BTimestamp);
   %     _ ->
   %            ?INFO_MSG("kabam_dev: No Log Edit/Delete/Note Event Message ~n",[]),
   %            void
   %end.

%% HueT
filter_room_packet(#xmlel{name= <<"message">>, attrs= Attrs} = Packet, _FromNick, _FromJID, _RoomJID) ->
   EditTag = xml:get_subtag(Packet,<<"replace">>),
   DeleteTag = xml:get_subtag(Packet,<<"delete">>),
   NoteTag = xml:get_subtag(Packet,<<"note">>),
   case {DeleteTag,NoteTag,EditTag} of 
      {false,false,false} ->
           case xml:get_attr_s(<<"type">>, Attrs) of 
                <<"groupchat">> ->
               TimeStamp = calendar:now_to_universal_time(now()), 
                   ?INFO_MSG("kabam_dev: add timestamp to packet: ~p ~n",[TimeStamp]),
               create_new_pack(Packet, TimeStamp);
                _ ->
                   Packet
           end;
      _ ->
           Packet
   end;
      
filter_room_packet(Packet,_FromNick, _FromJID, _RoomJID) ->
  Packet.

create_new_pack(#xmlel{attrs = Attrs} = Packet, Timestamp) ->
  {SDateTime, SZ} = jlib:timestamp_to_iso(Timestamp, utc),
  BTimestamp = list_to_binary(SDateTime ++ SZ),
  NewAttrs = lists:append(Attrs, [{<<"timestamp">>, BTimestamp}]),
  Packet#xmlel{attrs = NewAttrs}.

stb(String) ->
        case String of
          Val when is_list(Val) ->
            list_to_binary(Val);
      Val when is_integer(Val) ->
         integer_to_binary(Val);
          _ ->
              String
        end.

get_room_participants(GroupJid) ->
     mod_muc_room:get_room_participants(GroupJid).


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

sorted_str(JidStr1, JidStr2) when JidStr2 > JidStr1 ->
  << JidStr1/binary, JidStr2/binary >>;
sorted_str(JidStr1, JidStr2) ->
  << JidStr2/binary, JidStr1/binary >>.
