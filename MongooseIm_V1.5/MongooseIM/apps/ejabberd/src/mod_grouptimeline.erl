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
-include("mod_antbuddy_stanza.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% IQ handlers
-export([process_mam_iq/3]).
-export([process_iq_restart_room/3]).

%% Interface to mod_muc_room
-export([room_packet_hook/4]).

%% For development only
-compile(export_all).

%% Internal state
-record(state, {host}).

%% Macros
-define(PROCNAME, ejabberd_mod_group_timeline).
-define(PROC(Server), gen_mod:get_module_proc(Server, ?PROCNAME)).
-define(NS_RESTART_ROOM, <<"jabber:iq:restartroom">>).

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
    %ejabberd_redis:start_link([
    %        {worker_config, ejabberd_config:get_local_option({redis_opts, Host})}
    %        ]),
    %% Have the module to handle the MAM traffic
    mod_muc_iq:register_iq_handler(ConferenceHost, ?NS_MAM,
                                  ?MODULE, process_mam_iq),
    ?INFO_MSG("init: NS_RESTART_ROOM: ~p", [?NS_RESTART_ROOM]),
    mod_muc_iq:register_iq_handler(ConferenceHost, ?NS_RESTART_ROOM,
                                  ?MODULE, process_iq_restart_room),
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
    mod_muc_iq:unregister_iq_handler(State#state.host, ?NS_RESTART_ROOM),
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
  case xml:get_tag_attr_s(<<"userjid">>, Query) of
  BKiteUserJid when BKiteUserJid /= <<>> ->
    process_sm_iq_count_unread(From, GroupJid, IQ);
  _ ->
    {_WithJid, Start, End, Limit, Direction} = mam:resolve_request_params(From, Query),
    Result = process_mam(GroupJid, From, Start, End, Limit, Direction),
    result_iq(Result, IQ, 
              fun(Msg) ->
                  send_archive_message(From, GroupJid, Msg, From#jid.luser)
              end)
  end;

process_mam_iq(From, To, IQ) ->
  ?INFO_MSG("Unexpected request to MAM ~p",[{From, To, IQ}]),
  ignore.

process_iq_restart_room(_From, _Host, #iq{type = set, sub_el = Query} = IQ) ->
  ?INFO_MSG("process_iq_restart_room ~n", []),
  case xml:get_tag_attr_s(<<"roomjid">>, Query) of
  BRoomJid when BRoomJid /= <<>> ->
    RoomJid = jlib:binary_to_jid(BRoomJid),
    mod_muc:restart_room(RoomJid#jid.luser, RoomJid#jid.lserver),
    IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>,
             attrs = [{<<"xmlns">>, ?NS_RESTART_ROOM}, {<<"roomjid">>, BRoomJid}]}]};
  _ ->
    IQ#iq{type = error, sub_el = [#xmlel{name = <<"query">>,
             attrs = [{<<"xmlns">>, ?NS_RESTART_ROOM}]}]}
  end;

process_iq_restart_room(From, To, IQ) ->
  ?INFO_MSG("Unexpected request to MAM ~p",[{From, To, IQ}]),
  ignore.


process_sm_iq_count_unread(From, To, #iq{type = get, sub_el = SubEl} = IQ) ->
  ?INFO_MSG("process_sm_iq_count_unread: ~n", []),
  BKiteUserJid = xml:get_tag_attr_s(<<"userjid">>, SubEl),
  KiteUserJid = jlib:binary_to_jid(BKiteUserJid),
  Server = KiteUserJid#jid.lserver,
  KiteUserId = KiteUserJid#jid.luser,
  BObjectJID = jlib:jid_to_binary(To),
  ?INFO_MSG("BObjectJID: ~p~n", [BObjectJID]),
  {CountMessage, TimeStamp} =
  case mod_active_status_odbc:get_timestamp_status_user(Server, KiteUserId, 
                                Server, BObjectJID) of
    [] -> { 0, jlib:now_to_utc_binary(os:timestamp()) };
    {<<"0">>, Ts} ->
      { count_unreadmsg_of_kite_user(From, To, Ts), <<Ts/binary, "Z">> };
    _ -> { 0, jlib:now_to_utc_binary(os:timestamp()) }
  end,
  IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>,
             attrs = [{<<"xmlns">>, ?NS_MAM}, {<<"userjid">>, BKiteUserJid}
             , {<<"count">>, integer_to_binary(CountMessage)}
             , {<<"last_time">>, TimeStamp}]}]};

process_sm_iq_count_unread(_From, _To, #iq{sub_el = SubEl} = IQ) ->
  IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]}.

count_unreadmsg_of_kite_user(From, To, Ts) ->
  Timestamp = <<Ts/binary, "Z">>,
  UTCTime = jlib:datetime_binary_to_timestamp(Timestamp),
  ObjectId = To#jid.luser,
  UUID = 
  case binary:split(ObjectId, [<<"_">>]) of
    [ID, _] -> ID;
    [ObjectId] -> ObjectId;
    _ -> []
  end,
  length(mod_mongo:fetch([
                    {fromKey, ObjectId},
                    {time, [{gte, UTCTime}]}])).

  % if CountMessage > 0 ->
  %     Stanza = mod_antbuddy_stanza:count_unread_message(
  %                     CountMessage, Timestamp),
  %     ejabberd_router:route(To, From, Stanza);
  %   true ->
  %     nothing_to_do
 % end.


process_mam(GroupId, From, Start, End, Limit, Direction) ->
    get_page(GroupId, From, Start, End, Limit, Direction).

result_iq(Result, IQ, _SendResultsFun) ->
    case Result of
      {ok, Page} ->
        %%lists:foreach(SendResultsFun, Page),
        IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>,
									attrs = [{<<"xmlns">>, ?NS_MAM}],
        							children = Page}]};
      {error, _Reason} ->
        IQ#iq{type = error, sub_el = []} %%TODO: add more details to error handling
    end.

%% store with bot message
log_msg(message_bot, From, BodyMsg, RoomID, Ts, ExpandBody, Message_id, SubType) ->
    Doc = [{body, BodyMsg}, {?FROM_KEY, RoomID}, {id, Message_id}, {?RECEIVER_KEY, <<"null">>}, {?SENDER_KEY, From}, {expandBody, ExpandBody}, {isModified, <<"false">>}, {subtype, SubType}, {type, <<"groupchat">>},{time, Ts}, {<<"__v">>, 0} ],
    mod_mongo:insert(Doc);

%% store message with oembed
log_msg(message_oembed, From, BodyMsg, RoomID, Ts, OembedData, Message_id, SubType) ->
    OembedInfo = [{html,                xml:get_subtag_cdata(OembedData, ?OEMBED_HTML)}, 
                    {author_url,        xml:get_subtag_cdata(OembedData, ?AUTHOR_URL)}, 
                    {author_name,       xml:get_subtag_cdata(OembedData, ?AUTHOR_NAME)}, 
                    {thumbnail_height,  xml:get_subtag_cdata(OembedData, ?OEMBED_THUMBNAIL_HEIGHT)}, 
                    {type,              xml:get_subtag_cdata(OembedData, ?OEMBED_TYPE)}, 
                    {provider_name,     xml:get_subtag_cdata(OembedData, ?PROVIDER_NAME)}, 
                    {version,           xml:get_subtag_cdata(OembedData, ?OEMBED_VERSION)}, 
                    {thumbnail_url,     xml:get_subtag_cdata(OembedData, ?OEMBED_THUMBNAIL_URL)}, 
                    {url,               xml:get_subtag_cdata(OembedData, ?OEMBED_URL)}, 
                    {thumbnail_width,   xml:get_subtag_cdata(OembedData, ?OEMBED_THUMBNAIL_WIDTH)}, 
                    {mean_alpha,        xml:get_subtag_cdata(OembedData, ?OEMBED_MEAN_ALPHA)}, 
                    {title,             xml:get_subtag_cdata(OembedData, ?TITLE)}, 
                    {description,       xml:get_subtag_cdata(OembedData, ?DESCRIPTION)}, 
                    {provider_url,      xml:get_subtag_cdata(OembedData, ?PROVIDER_URL)}],
    Doc = [{?SENDER_KEY, From}, {body, BodyMsg}, {oembed, OembedInfo }, {?FROM_KEY, RoomID}, {?RECEIVER_KEY, <<"null">>}, {id, Message_id}, {type, <<"groupchat">>}, {subtype, SubType}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
    mod_mongo:insert(Doc);

%% store message with file
log_msg(message_file, From, BodyMsg, RoomID, Ts, FileData, Message_id, SubType) ->
    FileInfo = [{thumbnailUrl,          xml:get_subtag_cdata(FileData, ?FILE_THUMBNAIL_URL)}, 
                {thumbnailWidth,        xml:get_subtag_cdata(FileData, ?FILE_THUMBNAIL_WIDTH)}, 
                {thumbnailHeight,       xml:get_subtag_cdata(FileData, ?FILE_THUMBNAIL_HEIGHT)}, 
                {mimeType,              xml:get_subtag_cdata(FileData, ?MIMETYPE)}, 
                {fileUrl,               xml:get_subtag_cdata(FileData, ?FILE_URL)}, 
                {size,                  xml:get_subtag_cdata(FileData, ?FILE_SIZE)}, 
                {name,                  xml:get_subtag_cdata(FileData, ?FILE_NAME)}],
    Doc = [{?SENDER_KEY, From}, {body, BodyMsg}, {file, FileInfo}, {?FROM_KEY, RoomID}, {?RECEIVER_KEY, <<"null">>}, {id, Message_id}, {type, <<"groupchat">>}, {subtype, SubType}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
    mod_mongo:insert(Doc);

log_msg(beeiq, From, BodyMsg, RoomID, Ts, Message_id, SubType, KiteTag) ->
    BeeInfo = [{kite_app_name, xml:get_subtag_cdata(KiteTag, <<"kite_app_name">>)}
                , {avatar, xml:get_subtag_cdata(KiteTag, <<"avatar">>)}
                , {provider, xml:get_subtag_cdata(KiteTag, <<"provider">>)}
                , {type, xml:get_subtag_cdata(KiteTag, <<"type">>)}],
    Doc = [{?SENDER_KEY, From}, {body, BodyMsg}, {beeiq, BeeInfo}, {?FROM_KEY, RoomID}, {?RECEIVER_KEY, <<"null">>}, {id, Message_id}, {type, <<"groupchat">>}, {subtype, SubType}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
    mod_mongo:insert(Doc).

log_msg(beeiq, From, BodyMsg, RoomID, Ts, Message_id, SubType, KiteTag, FileData) ->
    BeeInfo = [{kite_app_name, xml:get_subtag_cdata(KiteTag, <<"kite_app_name">>)}
                , {avatar, xml:get_subtag_cdata(KiteTag, <<"avatar">>)}
                , {provider, xml:get_subtag_cdata(KiteTag, <<"provider">>)}
                , {type, xml:get_subtag_cdata(KiteTag, <<"type">>)}],
    FileInfo = [{thumbnailUrl,          xml:get_subtag_cdata(FileData, ?FILE_THUMBNAIL_URL)}, 
                {thumbnailWidth,        xml:get_subtag_cdata(FileData, ?FILE_THUMBNAIL_WIDTH)}, 
                {thumbnailHeight,       xml:get_subtag_cdata(FileData, ?FILE_THUMBNAIL_HEIGHT)}, 
                {mimeType,              xml:get_subtag_cdata(FileData, ?MIMETYPE)}, 
                {fileUrl,               xml:get_subtag_cdata(FileData, ?FILE_URL)}, 
                {size,                  xml:get_subtag_cdata(FileData, ?FILE_SIZE)}, 
                {name,                  xml:get_subtag_cdata(FileData, ?FILE_NAME)}],
    Doc = [{?SENDER_KEY, From}, {body, BodyMsg}, {file, FileInfo}, {beeiq, BeeInfo}, {?FROM_KEY, RoomID}, {?RECEIVER_KEY, <<"null">>}, {id, Message_id}, {type, <<"groupchat">>}, {subtype, SubType}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
    mod_mongo:insert(Doc).

%%store add message
log_msg(add_user, From, BodyMsg, RoomID, Ts, Message_id, SubType) ->
    Doc = [{?SENDER_KEY, From}, {body, BodyMsg}, {?FROM_KEY, RoomID}, {?RECEIVER_KEY, <<"null">>}, {id, Message_id}, {subtype, <<"welcome">>}, {type, <<"groupchat">>}, {subtype, SubType}, {time, Ts}, {<<"__v">>, 0}],
    mod_mongo:insert(Doc);

%% store normal message
log_msg(normal, From, BodyMsg, RoomID, Ts, Message_id, SubType) ->
    Doc = [{?SENDER_KEY, From}, {body, BodyMsg}, {?FROM_KEY, RoomID}, {?RECEIVER_KEY, <<"null">>}, {id, Message_id}, {type, <<"groupchat">>}, {subtype, SubType}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
    mod_mongo:insert(Doc).

%% Retrieval of the archive page specified by the timestamp range and the size.
%%
get_page(GroupId, From, StartTs, EndTs, PageSize, Direction) ->
    {Selector, Optional} = page_cmd(GroupId, StartTs, EndTs, PageSize, Direction),
    case mod_mongo:fetch_last_n(Selector, Optional) of        
        {error, Reason} ->
            error_logger:info_msg("Error in get_page", [Reason]),
            {error, Reason};
        Results -> 
        	Res = case Direction of 
                forward ->
                	%%lists:reverse(Results);
                    Results;
                backward ->
                	%%Results
                    lists:reverse(Results)
            end,
          	{ok, lists:map(fun(R) -> 
          			mod_antbuddy_stanza:convert_value_to_message(GroupId, From, R)
          		end, Res)}
  end.

%% Create the output for the archive message, and route it to the requesting Jid
send_archive_message(From, GroupJid, Msg, Nick) ->
    ejabberd_router:route(jlib:jid_replace_resource(GroupJid, Nick),
                           From, Msg).

page_cmd(GroupId, StartTs, EndTs, PageSize, Direction) ->
    StartTsBin = mam:mamts_to_timestamp(StartTs),
    EndTsBin = mam:mamts_to_timestamp(EndTs),
    Selector = [{?FROM_KEY, GroupId#jid.luser}, 
                {time, [{gte, StartTsBin}, 
                        {lte, EndTsBin}]
                }],
    Orderby = case Direction of
    	forward -> asc;
    	backward -> desc
    end,
    Optional = [{limit, PageSize}, {orderby, [{time, Orderby}]}],
    {Selector, Optional}.

room_packet_hook(_FromNick, FromJID, RoomJid, Packet) ->
  {MsgId, Timestamp} = get_message_id_and_timestamp(Packet),
  case xml:get_subtag(Packet, ?EVENT) of
    false ->
    	BodyMsg = xml:get_subtag_cdata(Packet, <<"body">>),
      SubType =
      case xml:get_tag_attr_s(<<"subtype">>, Packet) of
        St when St /= <<>> -> St;
        _ -> <<>>
      end,
    	case {xml:get_subtag(Packet, <<"file">>),
    		xml:get_subtag(Packet, <<"oembed">>),
    		xml:get_subtag(Packet, <<"expandbody">>)} of
    		%% normal message and add user message
    		{false, false, false} ->
    			case SubType of
    				<<"welcome">> ->
    					log_msg(add_user, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, Timestamp, MsgId, SubType);
    				_ ->
    					case BodyMsg of
    						<<>> -> Packet;
    						_ ->
    							%log_msg(normal, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, Timestamp, MsgId, SubType)
                  check_and_log_beeiq_msg(normal, FromJID, BodyMsg, RoomJid, Timestamp, MsgId, SubType, Packet)
    					end
    			end;
    		%% message with file
    		{FileData, false, false} ->
    			%log_msg(message_file, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, Timestamp, FileData, MsgId, SubType);
          check_and_log_beeiq_msg(message_file, FromJID, BodyMsg, RoomJid, Timestamp, FileData, MsgId, SubType, Packet);
    			%% message with oembed
    			{false, OembedData, false} ->
    				log_msg(message_oembed, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, Timestamp, OembedData, MsgId, SubType);
    			%% bot message
    			{false, false, _BotData} ->
    				ExpandBody = xml:get_subtag_cdata(Packet, <<"expandbody">>),
    				log_msg(message_bot, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, Timestamp, ExpandBody, MsgId, SubType);
    		_ ->
    			Packet
    	end;
    Event ->
      ?INFO_MSG("#69 Event message ~p~n", [Event]),
      case  xml:get_subtag(Packet, <<"kite">>) of
        false -> ok;
        EventKiteTag ->
          case xml:get_tag_attr_s(<<"type">>, Event) of
              EventType -> 
                  case lists:member(EventType, ?KITE_EVENTS) of
                    true -> 
                      mod_beeiq_handler:process_event_message(EventType, MsgId, Timestamp, RoomJid, EventKiteTag);
                    _ -> ok
                  end;
              _ ->
                  ok
          end
      end
  end.

check_and_log_beeiq_msg(normal, FromJID, BodyMsg, RoomJid, Timestamp, MsgId, SubType, Packet) ->
  case  xml:get_subtag(Packet, <<"kite">>) of
    false -> 
      log_msg(normal, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, Timestamp, MsgId, SubType);
    KiteTag -> 
      case xml:get_subtag_cdata(KiteTag, <<"provider">>) of
        <<"BeeIQ">> ->
          log_msg(beeiq, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, Timestamp, MsgId, SubType, KiteTag);
        _ ->
          mod_beeiq_handler:process_kite_message(Packet, BodyMsg, MsgId, Timestamp, RoomJid, KiteTag),
          log_msg(normal, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, Timestamp, MsgId, SubType)
      end
  end.

check_and_log_beeiq_msg(message_file, FromJID, BodyMsg, RoomJid, Timestamp, FileData, MsgId, SubType, Packet) ->
  case  xml:get_subtag(Packet, <<"kite">>) of
    false -> 
      log_msg(message_file, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, Timestamp, FileData, MsgId, SubType);
    KiteTag -> 
      case xml:get_subtag_cdata(KiteTag, <<"provider">>) of
        <<"BeeIQ">> ->
          log_msg(beeiq, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, Timestamp, MsgId, SubType, KiteTag, FileData);
        _ ->
          mod_beeiq_handler:process_kite_message(Packet, BodyMsg, MsgId, Timestamp, RoomJid, KiteTag),
          log_msg(message_file, FromJID#jid.luser, BodyMsg, RoomJid#jid.luser, Timestamp, FileData, MsgId, SubType)
      end
  end.

%% HueT
filter_room_packet(#xmlel{name= <<"message">>, attrs= Attrs} = Packet, 
						_FromNick, FromJID, RoomJID) ->
   EditTag = xml:get_subtag(Packet,<<"replace">>),
   DeleteTag = xml:get_subtag(Packet,<<"delete">>),
   NoteTag = xml:get_subtag(Packet,<<"note">>),
   [_, Server] = binary:split(RoomJID#jid.lserver, <<".">>),
   case {DeleteTag,NoteTag,EditTag} of 
      {false,false,false} ->
           case xml:get_attr_s(<<"type">>, Attrs) of 
                <<"groupchat">> ->
					TimeStamp = jlib:now_to_utc_binary_milisecs(os:timestamp()), 
					NewPacket = create_new_pack(Packet, TimeStamp),
                    ejabberd_hooks:run(push_notification, Server, 
                                                [FromJID, RoomJID, NewPacket]),
                    NewPacket;
                _ ->
                   Packet
           end;
      _ ->
          ejabberd_hooks:run(push_notification, Server, 
                                                [FromJID, RoomJID, Packet]),
           Packet
   end;
      
filter_room_packet(Packet,_FromNick, _FromJID, _RoomJID) ->
  Packet.

create_new_pack(#xmlel{attrs = Attrs} = Packet, BTimestamp) ->
	Attrs1 = case xml:get_tag_attr_s(<<"id">>, Packet) of
		<<>> ->
			MsgId = mod_antbuddy_stanza:item_to_binary(
						mod_mam_utils:generate_message_id()),
			lists:append(Attrs, [{<<"id">>, MsgId}]);
		_ -> Attrs
	end,
	NewAttrs = lists:append(Attrs1, [{<<"timestamp">>, BTimestamp}]),	
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

get_timestamp() ->
    UTimestamp  = calendar:now_to_universal_time(os:timestamp()),
    {SDateTime, SZ} = jlib:timestamp_to_iso(UTimestamp, utc),
    list_to_binary(SDateTime ++ SZ).

get_message_id_and_timestamp(Packet) ->
    Msg = case xml:get_tag_attr_s(<<"id">>, Packet) of
        <<>> ->
            mod_antbuddy_stanza:item_to_binary(
            		mod_mam_utils:generate_message_id());
		Id -> Id
    end,
    Timestamp = case xml:get_tag_attr_s(<<"timestamp">>, Packet) of
        <<>> ->
            os:timestamp();
		    Time -> 
            jlib:datetime_binary_to_timestamp(Time)
    end,
    {Msg, Timestamp}.

