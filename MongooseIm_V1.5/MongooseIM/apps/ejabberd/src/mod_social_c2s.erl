-module(mod_social_c2s).

-behavior(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/2]).
-export([instance/2]).
-export([get_publicaccount_info/0, get_facebook_info_with_msgid/2]).
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_social.hrl").
-include("mod_antbuddy_stanza.hrl").

-define(GET_ACCOUNT_INFO_API, "https://chatapi.viber.com/pa/get_account_info").

instance(#social_session{id = ConversationId,  
                    user_account = UserAccount ,
                    bot_account = BotAccount,
                    conversation = Conversation,
                    channel_id = ChannelId,
                    service_url = ServiceUrl } = Session, WebService) -> 

    ProcName = 
    case ChannelId of
        <<"viber">> -> 
            mod_social_util:proc_name(get(<<"id">>, BotAccount), get(<<"id">>, UserAccount));
        _ ->
            mod_social_util:proc_name(ChannelId, ConversationId)
    end,
    ChildSpec = {ProcName,                                                          %%ChildId, 
                {?MODULE, start_link, [ProcName, #state{id = ConversationId,  
                                                        user_account = UserAccount ,
                                                        bot_account = BotAccount,
                                                        conversation = Conversation,
                                                        channel_id = ChannelId,
                                                        service_url = ServiceUrl,
                                                        web_base = WebService}]},  %%StartFunc
                 transient,                                                          %%Restart
                 2000,                                                               %%Shutdown                          
                 worker,                                                             %%Type 
                 [?MODULE]},                                                     %%Modules
    
    case mod_social_service:start_child(mod_social_sup, ChildSpec)  of
        {ok, NewPid} ->
			?INFO_MSG("#69 Create new process for user: ~p, proc_name: ~p~n", [UserAccount, ProcName]), 
            % Store init state of c2s to db
            mod_social_session:add_session(Session),
            {ok, NewPid};
        {error, Error} -> 
            ?ERROR_MSG("#69 Cannot start create pid for user: ~p, proc_name: ~p, error: ~p~n", [UserAccount, ProcName, Error]),
            error;
        _ -> 
            ?ERROR_MSG("#69 Cannot start create pid for user: ~p, proc_name: ~p ~n", [UserAccount, ProcName]),
            error
    end;
instance(_, _) ->
    error.

start_link(ProcName, State) ->
    gen_server:start_link({local, ProcName}, ?MODULE, State, []).

init(State) -> 
    ?INFO_MSG("init ~n", []),
    self() ! init,
    {ok, State}.

handle_info(init, State) ->
    ?INFO_MSG("channel_id: ~p~n", [State#state.channel_id]),
    %% Get Bot info and store to state data
    case State#state.channel_id of
        <<"viber">> ->
            %% Viber
            UserName = get(<<"name">>, State#state.user_account),
            UserID = get(<<"id">>, State#state.user_account),
            Question = get(<<"question">>, State#state.conversation),
            ?INFO_MSG("Question: ~p~n", [Question]),
            BotInfo = State#state.bot_account,
            WebApi = get_web_api(State#state.channel_id, State#state.web_base, BotInfo),
            Fingerprint = mod_social_util:sha1(UserID),
            %SocialBody = get_social_info(<<"viber">>, State),
            SocialBody = State#state.user_account,
            KiteBody = [{<<"fingerprint">>, Fingerprint}, 
                            {<<"email">>, <<>>}, 
                            %{<<"name">>, UserName}, 
                            {<<"question">>, Question}, 
                            {<<"phone">>, <<>>}, 
                            {<<"origin">>, <<"viber">>},
                            {<<"referrer">>, <<>>}],
            Body = KiteBody ++ SocialBody,
            case send_request_create_room(WebApi, jsx:encode(Body), ?RETRY) of
                        {ok, Result} ->

                            BJID = get(<<"guestJid">>, Result),
                            BRoomJID = get(<<"roomJid">>, Result),
                            RoomJID = jlib:binary_to_jid(BRoomJID),
                            % store (Pid, UserJID) to ejbabberd_sm
                            SID = {os:timestamp(), self()},
                            Info = [{conn, http_bot_framework},
                                    {auth_module, auth_anonymous}],
                            Resource = mod_social_util:generate_resource(State#state.id),
                            NewState = State#state{ jid = jlib:binary_to_jid(<<BJID/binary, "/", Resource/binary>>),
                                        token = get(<<"guestToken">>, Result),
                                        room_jid = RoomJID,
                                        room_pin = get(<<"roomPin">>, Result),
                                        sid = SID},
                            ejabberd_sm:open_session(
                              SID, (NewState#state.jid)#jid.luser, 
                                    (NewState#state.jid)#jid.lserver, 
                                    (NewState#state.jid)#jid.lresource, Info),
                            %% presence to join room
                            NewStateData = case mnesia:dirty_read(muc_online_room, {RoomJID#jid.luser, RoomJID#jid.lserver}) of
                                [] ->
                                    NewState;
                                [R] ->
                                    Pid = R#muc_online_room.pid,
                                    {Nick, _, _} = jlib:jid_tolower(NewState#state.jid),
                                    Packet = mod_social_util:make_presence_join_room(NewState#state.room_pin, 
                                                                    jlib:jid_replace_resource(NewState#state.room_jid, Nick)),
                                    ejabberd_router:route(NewState#state.jid, 
                                                        jlib:jid_replace_resource(NewState#state.room_jid, Nick), %Nick
                                                        Packet),
                                    NewState#state{room_pid = Pid}
                            end,
                            ?INFO_MSG("1. send_request_create_room: ~n", []),
                            {noreply, NewStateData#state{access_token = #bot_access_token{expires_in = <<>>}}};
                        _ ->
                            ?INFO_MSG("2. send_request_create_room: ~n", []),
                            ?ERROR_MSG("#69: Can't create room to communicate with social network ~n", []),
                            {stop, normal, State}
                    end;
        _ ->
            BotId = get(<<"name">>, State#state.bot_account),
            case get_bot_info(BotId) of
                [] ->
        			?ERROR_MSG("#69: Can't find bot for this room ~n", []),
                    {stop, normal, State};
                BotConfig ->
                    MSAppID = get(<<"ms_app_id">>, BotConfig),

                    MSAppSecret = get(<<"ms_app_secret">>, BotConfig),
                    FbToken = get(<<"fb_token">>, BotConfig),
                    %KiteAppId =  get(<<"kite_app_id">>, BotConfig),
        			%KiteAppIdSkype = get(<<"kite_app_id_for_skype">>, BotConfig),
                    %WebApi = State#state.web_base ++ binary_to_list(KiteAppId) ++ "/request-support",
        			WebApi = get_web_api(State#state.channel_id, State#state.web_base, BotConfig),
                    ?DEBUG("#69 BotId: ~p, MsAppSecret: ~p, MSAppID: ~p, FbToken: ~p, WebApi:~p~n", 
                                        [BotId, MSAppID, MSAppSecret, FbToken, WebApi]),

                    {AccesToken, ExpireIn, TokenType, CreateTimestampAt} = 
                                get_access_token(MSAppID, MSAppSecret, ?RETRY),

                    State0 = State#state{app_id = BotId,
                                        ms_app_id = MSAppID,
                                        ms_app_secret = MSAppSecret,
                                        fb_token = FbToken,
                                        web_api = WebApi,
                                        jwks_uri = get_openid_document(),
                                        access_token = #bot_access_token{
                                                    access_token = AccesToken,
                                                    token_type =TokenType,
                                                    expires_in = ExpireIn,
                                                    create_token_at = CreateTimestampAt
                                        }}, 

                    % call WebAPI to create room, store Room/User JID Info to state
                    %%OriginType = Get(<<"channelId">>, State0#state.user_account),
                    % UserName = get(<<"name">>, State0#state.user_account),
                    Question = get(<<"question">>, State0#state.conversation),
                    MsgId = get(<<"msg_id">>, State0#state.conversation),
                    Fingerprint = mod_social_util:sha1(State0#state.id),
        			SocialBody = get_social_info(State#state.channel_id, State0),
                    UserName = get_failover_username(State0#state.user_account, SocialBody, MsgId, FbToken),
                    KiteBody = [{<<"fingerprint">>, Fingerprint}, 
                            {<<"email">>, <<>>}, 
                            {<<"name">>, UserName}, 
                            {<<"question">>, Question}, 
                            {<<"phone">>, <<>>}, 
                            {<<"origin">>, State0#state.channel_id},
                            {<<"referrer">>, <<>>}],
        			Body = KiteBody ++ SocialBody,
                    case send_request_create_room(State0#state.web_api, jsx:encode(Body), ?RETRY) of
                        {ok, Result} ->
                            BJID = get(<<"guestJid">>, Result),
                            BRoomJID = get(<<"roomJid">>, Result),
                            RoomJID = jlib:binary_to_jid(BRoomJID),
                            % store (Pid, UserJID) to ejbabberd_sm
                            SID = {os:timestamp(), self()},
                            Info = [{conn, http_bot_framework},
                                    {auth_module, auth_anonymous}],
                            Resource = mod_social_util:generate_resource(State0#state.id),
                            NewState = State0#state{ jid = jlib:binary_to_jid(<<BJID/binary, "/", Resource/binary>>),
                                        token = get(<<"guestToken">>, Result),
                                        room_jid = RoomJID,
                                        room_pin = get(<<"roomPin">>, Result),
                                        sid = SID},
                            ejabberd_sm:open_session(
                              SID, (NewState#state.jid)#jid.luser, 
                                    (NewState#state.jid)#jid.lserver, 
                                    (NewState#state.jid)#jid.lresource, Info),
                            %% presence to join room
                            NewStateData = case mnesia:dirty_read(muc_online_room, {RoomJID#jid.luser, RoomJID#jid.lserver}) of
                                [] ->
                                    NewState;
                                [R] ->
                                    Pid = R#muc_online_room.pid,
                                    {Nick, _, _} = jlib:jid_tolower(NewState#state.jid),
                                    Packet = mod_social_util:make_presence_join_room(NewState#state.room_pin, 
                                                                    jlib:jid_replace_resource(NewState#state.room_jid, Nick)),
                                    ejabberd_router:route(NewState#state.jid, 
                                                        jlib:jid_replace_resource(NewState#state.room_jid, Nick), %Nick
                                                        Packet),
                                    NewState#state{room_pid = Pid}
                            end,
                            {noreply, NewStateData};
                        _ ->
                            ?ERROR_MSG("#69: Can't create room to communicate with social network ~n", []),
                            {stop, normal, State0}
                    end
                       
            end
    end;

handle_info({message, _Message}, #state{jid = 'undefined'} = State) ->
       ?ERROR_MSG("#69: JID is undefined ~n", []),
       {stop, normal, State};
handle_info({message, _Message}, #state{room_jid = 'undefined'} = State) ->
    ?ERROR_MSG("#69: RoomJID is undefined ~n", []),
       {stop, normal, State};
%% send message from social channel to Antbuddy
handle_info({message, Message}, State) -> 
    % send Message to Room
     ?DEBUG("joined_room: ~p~n", [State]),
     NewState = case check_authorize(Message#message.authorization, State) of 
        true -> 
           if State#state.joined_room ->
                                send_message_to_room({message, Message}, State),
                                State;
                          not State#state.joined_room -> 
                                State#state{message_queue = queue:in({message, Message}, State#state.message_queue)}
                        end;
        _ -> 
            ?ERROR_MSG("#69 Check authorization fail : ~p~n", [Message]),
            State
    end,
    {noreply, NewState};

%% send message from Antbuddy to social channel
handle_info({route, From, To, #xmlel{name = <<"message">>} = Packet}, State) -> 
    % check if message is sent by supporter
    % if true, send message back to bot api
    ?INFO_MSG("handle_info: message~n", []),
    ?DEBUG("#69 From: ~p, To: ~p, Packet:~p~n", [From, To, Packet]),
    SubType = xml:get_tag_attr_s(<<"subtype">>, Packet),
    {_, _, FromNick} = jlib:jid_tolower(From),
    
    if FromNick == To#jid.luser -> 
            {noreply, State};
        true ->
            % check kite close room event
            case xml:get_subtag(Packet, ?EVENT) of 
                false -> 
                    case State#state.channel_id of
                        <<"viber">> ->
                            Content =  xml:get_subtag_cdata(Packet, <<"body">>),
                            Attachments = case xml:get_subtag(Packet, <<"file">>) of 
                                        false -> 
                                            [];
                                        File -> 
                                            FileName = xml:get_subtag_cdata(File, ?FILE_NAME),
                                            FileType = xml:get_subtag_cdata(File, ?MIMETYPE),
                                            FileUrl = xml:get_subtag_cdata(File, ?FILE_URL),
                                            FileThumbnail = xml:get_subtag_cdata(File, ?FILE_THUMBNAIL_URL),
                                            FileSize = xml:get_subtag_cdata(File, ?FILE_SIZE),
                                            [ {<<"contentType">>, FileType},
                                              {<<"contentUrl">>, FileUrl},
                                              {<<"name">>, FileName},
                                              {<<"thumbnailUrl">>, FileThumbnail},
                                              {<<"size">>, FileSize}]

                                    end,
                            ?INFO_MSG("log viber reply text: ~ts; SubType: ~p~n", [Content, SubType]),
                            ?DEBUG("Attachments: ~p~n", [Attachments]),
                            if Content == <<>> andalso Attachments == []-> no_reply;
                                true ->
                                    if SubType /= <<"internal">> ->
                                        mod_social_util:make_reply_and_send_viber_message(From#jid.luser, 
                                            State#state.user_account, State#state.bot_account, Content, Attachments);
                                    true ->
                                        noreply
                                    end
                            end,
                            {noreply, State};
                        _ ->
                            % Normal message
                            ReplyContent0 = case xml:get_subtag(Packet, <<"file">>) of 
                                        false -> 
                                            #reply{attachments=[]};
                                        File -> 
                                            FileName = xml:get_subtag_cdata(File, ?FILE_NAME),
                                            FileType = xml:get_subtag_cdata(File, ?MIMETYPE),
                                            FileUrl = xml:get_subtag_cdata(File, ?FILE_URL),
                                            FileThumbnail = xml:get_subtag_cdata(File, ?FILE_THUMBNAIL_URL),
                                            FileContent = get_content_file(FileUrl),
                                            %?INFO_MSG("FileContent: ~p~n", [FileContent]),
                                            ActtachFile = [ {<<"contentType">>, FileType},
                                              {<<"contentUrl">>, FileUrl},
                                              {<<"name">>, FileName},
                                              {<<"thumbnailUrl">>, FileThumbnail}],
                                            %?INFO_MSG("ActtachFile: ~p~n", [ActtachFile]),
                                            #reply{attachments=[ActtachFile]}

                                    end,
                            Content =  xml:get_subtag_cdata(Packet, <<"body">>),

                            ReplyContent = ReplyContent0#reply{text = Content},

                            ?DEBUG("#69 ReplyContent: ~p~n", [ReplyContent]),
                            State0 = case mod_social_util:is_expired_time((State#state.access_token)#bot_access_token.expires_in,
                                                                 (State#state.access_token)#bot_access_token.create_token_at) of 
                                true -> 
                                    {AccesToken, ExpireIn, TokenType, CreateTimestampAt} = 
                                        get_access_token(State#state.ms_app_id, State#state.ms_app_secret, ?RETRY),
                                    State#state{ access_token = #bot_access_token{
                                                 access_token = AccesToken,
                                                 token_type =TokenType,
                                                 expires_in = ExpireIn,
                                                 create_token_at = CreateTimestampAt
                                                 } };
                                false ->
                                    State    
                            end,
                            if 
                                ReplyContent#reply.text == <<>> andalso ReplyContent#reply.attachments == [] -> 
                                    no_reply;
                                true ->
                                    if SubType /= <<"internal">> -> 
                                        ConversationUrl = binary_to_list(State#state.service_url) ++ ?CONVERSION_URI ++
                                                            binary_to_list(State#state.id) ++ "/activities",

                                        ReplyMsg = mod_social_util:make_reply_message(State0#state.channel_id
            																		, State0#state.bot_account
            																		, State0#state.user_account
            																		, State0#state.conversation
            																		, ReplyContent),
                                        ?INFO_MSG("ReplyMsg: Content: ~ts~n", [Content]),
                                        mod_social_util:send_reply_message(ConversationUrl, State0#state.access_token, ReplyMsg, ?RETRY);
                                    true ->
                                        noreply
                                    end
                            end,
                            {noreply, State0}
                    end;
                Event ->
                    ?INFO_MSG("#69 Event message ~p~n", [Event]),
                    case xml:get_tag_attr_s(<<"type">>, Event) of
                        <<"kite-close">> -> 
                            {stop, normal, State};
                        _ ->
                            {noreply, State}
                    end
            end
    end;

handle_info({route, From, To, #xmlel{name = <<"presence">>} = Packet}, State) -> 
    ?INFO_MSG("handle_info: presence: ~n", []),
    ?DEBUG("#69 From: ~p, To: ~p, Packet:~p~n", [From, To, Packet]),
    {_, _, FromNick} = jlib:jid_tolower(From),
    NewState =  if  FromNick == To#jid.luser ->  
                        case xml:get_path_s(Packet, [{elem, <<"x">>}, {elem, <<"status">>}, {attr, <<"code">>}]) of 
                            <<"110">> -> 
                                ?INFO_MSG("User:~p, with Jid: ~p, join room ~p success~n", [State#state.id, State#state.jid, State#state.room_jid]),
                                
                                case queue:is_empty(State#state.message_queue) of
                                    true ->
                                        send_no_thing;
                                    _ -> 
                                        lists:map(fun ({message, Message}) ->
                                                    send_message_to_room({message, Message}, State)
   
                                                  end, queue:to_list(State#state.message_queue))
                                end,
                                State#state{joined_room = true, message_queue=queue:new()};
                            Status    -> 
                                ?ERROR_MSG("#69 User ~p with Jid: ~p, cannot join room ~p, status:~p~n", [State#state.id, State#state.jid, State#state.room_jid, Status]),
                                State
                        end; 
                    true -> 
                        State
                end,
    {noreply, NewState};

handle_info({route, From, To, Packet}, State) ->    
    ?INFO_MSG("#69 Unhandle message, From: ~p, To: ~p, Packet: ~p~n", [From, To, Packet]),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(Reason, StateData) when StateData#state.jid =:= undefined -> 
    ?INFO_MSG("#69 Close session for reason: ~p~n", [Reason]),
	remove_c2s_session(StateData);
terminate(Reason, StateData) -> 
    ?INFO_MSG("#69 Close session for ~p, reason: ~p~n", [jlib:jid_to_binary(StateData#state.jid), Reason]),
    {U, S, R} = jlib:jid_to_lower(StateData#state.jid),
    ejabberd_sm:close_session(StateData#state.sid, U, S, R),
    %% presence to leave room
    Packet = #xmlel{name = <<"presence">>,
                attrs = [{<<"type">>, <<"unavailable">>}]},
    ejabberd_router:route(StateData#state.jid,
                        jlib:jid_replace_resource(StateData#state.room_jid, U),
                        Packet),
	remove_c2s_session(StateData).

remove_c2s_session(StateData) ->
	%remove c2s session state from db
	mod_social_session:delete_session(StateData#state.id),
    %ProcName = mod_social_util:proc_name(StateData#state.channel_id, StateData#state.id),
    ProcName = 
    case StateData#state.channel_id of
        <<"viber">> -> 
            mod_social_util:proc_name(get(<<"id">>, StateData#state.bot_account), get(<<"id">>, StateData#state.user_account));
        _ ->
            mod_social_util:proc_name(StateData#state.channel_id, StateData#state.id)
    end,
	%remove process c2s
    mod_social_session:delete_process_social_c2s(ProcName),
	% remove pid from supervisor tree
    mod_social_service:delete_child(mod_social_sup, ProcName).

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Request, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(Key, List) ->
	case proplists:get_value(Key, List) of
		undefined ->
			<<>>;
		Value ->
			Value
	end.

get_content_file(FileUrl) ->
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} = httpc:request(get, {binary_to_list(FileUrl), []}, [], []),
    base64:encode(Body).
    %list_to_binary(Body).

send_request_create_room(_, _, 0) ->
    no_request_create_room;
send_request_create_room(Url, Body, NumberOfRetry) ->
    try httpc:request(post, {Url, 
                            [{"Content-Type", ?CONTENT_TYPE_JSON}], 
                            ?CONTENT_TYPE_JSON, Body}, [{timeout, ?TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, Result}} ->
            case catch jsx:decode(list_to_binary(Result)) of
                {'EXIT', Msg} -> 
                    ?ERROR_MSG("#69 Cannot decode response: ~p, reason: ~p~n", [Result, Msg]),
                    error;
                Content -> 
                        {ok, Content}
            end;            
        Err ->
            ?ERROR_MSG("#69 Request create room fail, url: ~p, body: ~p, error: ~p~n", [Url, Body, Err]),
            send_request_create_room(Url, Body, NumberOfRetry - 1)
    catch
        _Exception:Reason -> 
            ?ERROR_MSG("#69 Reason: ~p~n", [Reason]),
            send_request_create_room(Url, Body, NumberOfRetry - 1)
    end.

send_message_to_room({message, #message{text = Text, attachments = Attachments}}, 
                    #state{jid=From
						, room_jid=Room
						, room_pid=RoomPid
						, channel_id=ChannelId}=State) ->
    Fsend = fun(Id, Body, Fmsg) -> 
                %Packet = mod_social_util:make_xmpp_message(Id, From, Room, Body),
				Packet = Fmsg(Id, From, Room, Body),
                ?DEBUG("#69 Packet: ~p ~n", [Packet]),
                mod_muc_room:route(RoomPid, From, <<>>, Packet)
            end,
    %case Text of
    %    <<"">> -> 
    %        send_no_thing;
    %    _ -> 
    %        Fsend(integer_to_binary(mod_mam_utils:generate_message_id()), Text
	%			, fun mod_social_util:make_xmpp_message/4)
    %end,
    ?INFO_MSG("send_message_to_room: ~n", []),
	send_text_message_to_room(ChannelId, Text, Fsend),

	send_attachment_message_to_room(State, Attachments, Fsend).

send_text_message_to_room(_ChannelId, <<"">>, _Fsend) ->
	send_no_thing;
send_text_message_to_room(<<"skype">>, Text, Fsend) ->
	Message = get_content_skype_message(Text),
	send_text_message_to_room(<<>>, Message, Fsend);
send_text_message_to_room(<<"viber">>, Text, Fsend) ->
    send_text_message_to_room(<<>>, Text, Fsend);

send_text_message_to_room(_ChannelId, Text, Fsend) ->
    Fsend(integer_to_binary(mod_mam_utils:generate_message_id()), Text
            , fun mod_social_util:make_xmpp_message/4).

send_attachment_message_to_room(_State, [], _Fsend) ->
       send_no_thing;
send_attachment_message_to_room(#state{channel_id= <<"facebook">>}, Attachments, Fsend) ->
       lists:map(fun(Attachment) -> 
                       % Handle attachment here
                       % Currently, just send url of attachment
                       Fsend(integer_to_binary(mod_mam_utils:generate_message_id())
							, Attachment#attachment.url
							, fun mod_social_util:make_xmpp_message/4)
                 end,  Attachments);

send_attachment_message_to_room(#state{channel_id= <<"viber">>
                                    , bot_account = BotAccount, web_base=WebApi}, Attachments, Fsend) ->
        
        lists:map(fun(Attachment) ->
               % Handle attachment here
               % Get binary data from skype
                MsgId = mod_mam_utils:generate_message_id(),
                AccessToken = get(<<"auth_token">>, BotAccount),
                
               case mod_social_util:get_attachment_from_viber(Attachment, AccessToken, WebApi) of
                    [] ->
                        Fsend(integer_to_binary(MsgId)
                            , Attachment#attachment.url
                            , fun mod_social_util:make_xmpp_message/4);
                    FileInfo when is_list(FileInfo) ->
                       Fsend(integer_to_binary(MsgId), FileInfo
                            , fun mod_social_util:make_attachment_xmpp_message/4);
                    _ ->
                        Fsend(integer_to_binary(MsgId)
                            , Attachment#attachment.url
                            , fun mod_social_util:make_xmpp_message/4)
                end
         end,  Attachments);

send_attachment_message_to_room(#state{channel_id= <<"skype">>
                                    , access_token= AccessToken, web_base=WebApi}, Attachments, Fsend) ->
       lists:map(fun(Attachment) ->
               % Handle attachment here
               % Get binary data from skype
				MsgId = mod_mam_utils:generate_message_id(),
               case mod_social_util:get_attachment_from_skype(Attachment, AccessToken, WebApi) of
					[] ->
						Fsend(integer_to_binary(MsgId)
                            , Attachment#attachment.url
                            , fun mod_social_util:make_xmpp_message/4);
					FileInfo when is_list(FileInfo) ->
					   Fsend(integer_to_binary(MsgId), FileInfo
							, fun mod_social_util:make_attachment_xmpp_message/4);
					_ ->
						Fsend(integer_to_binary(MsgId)
                            , Attachment#attachment.url
                            , fun mod_social_util:make_xmpp_message/4)
				end
         end,  Attachments);
send_attachment_message_to_room(_State, _Attachments, _Fsend) ->
       send_no_thing.

get_content_skype_message(Text) when is_binary(Text) ->
	get_content_skype_message(binary_to_list(Text));
get_content_skype_message(Text) when is_list(Text) ->
	%% find a href in text content
	Msg = case string:str(Text, "<a href=") of
		0 ->
			Text;
		_ ->
			%% remove all a href out of text content
			remove_html_link_code(Text)	
	end,
	list_to_binary(Msg);
get_content_skype_message(_Text) ->
	<<"">>.

remove_html_link_code(Text) ->
	TokenList = string:tokens(Text, "<>"),
	RemovedCode = lists:filter(fun(E) -> 
			AHref = string:str(E, "a href"), 
			if AHref > 0 -> false; 
				E == "/a" -> false; 
				true -> true 
			end end, TokenList),
	lists:flatten(RemovedCode).

get_bot_info(BotId) ->
    Selector = [{<<"bot_app_id">>, BotId}],
    case ets:lookup(track_social_bot, BotId) of
        [{_, BotCacheInfo}] -> BotCacheInfo;
        _ ->
            case mod_mongo:fetch_social_bot(Selector) of
                [] ->
                    [];
                BotInfoL ->
                    BotInfo = lists:last(BotInfoL),
                    ets:insert(track_social_bot, {BotId, BotInfo}),
                    BotInfo  
            end
    end.

get_access_token(MSAppID, MSAppSecret, NumberOfRetry) ->
    Body = "grant_type=" ++ ?GRANT_TYPE ++ "&" ++
        "client_id=" ++ binary_to_list(MSAppID) ++ "&" ++
        "client_secret=" ++ binary_to_list(MSAppSecret) ++ "&" ++
        "scope=" ++ ?SCOPE,
    get_access_token(Body, NumberOfRetry).

get_access_token( _, 0) ->
    {<<>>, <<>>, <<>>, 0};
get_access_token(Body, NumberOfRetry) ->
    try httpc:request(post, {?TOKEN_URL, 
                            [{"Content-Type", ?CONTENT_TYPE_FORM_URLENCODED}], 
                            ?CONTENT_TYPE_FORM_URLENCODED, Body}, [{timeout, ?TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, Result}} ->
            case catch jsx:decode(list_to_binary(Result)) of
                {'EXIT', Msg} -> 
                    ?ERROR_MSG("#69 Cannot decode response: ~p, reason: ~p~n", [Result, Msg]),
                    {<<>>, <<>>, <<>>, 0};
                Content ->
                    AccesToken = proplists:get_value(<<"access_token">>, Content),
                    ExpireIn = proplists:get_value(<<"expires_in">>, Content),
                    TokenType = proplists:get_value(<<"token_type">>, Content),
                    {AccesToken, ExpireIn, TokenType, mod_social_util:get_ms_timestamp()}
            end;            
        _Err ->
            get_access_token(Body, NumberOfRetry - 1)
    catch
        _Exception:Reason -> 
            ?ERROR_MSG("Reason: ~p~n", [Reason]),
            get_access_token(Body, NumberOfRetry - 1)
    end.

get_openid_document() ->
    case mod_social_util:request_http_get(?OPEN_ID_URL, <<>>) of
        <<>> ->
            <<>>;
        Result ->
            proplists:get_value(<<"jwks_uri">>, Result)
    end.

get_list_of_valid_signing_keys(Jwks_Uri) 
                    when is_binary(Jwks_Uri) ->
    get_list_of_valid_signing_keys(binary_to_list(Jwks_Uri));
get_list_of_valid_signing_keys(Jwks_Uri) ->
    case mod_social_util:request_http_get(Jwks_Uri, []) of
        [] ->
            [];
        Result ->
            ?DEBUG("#69 Result: ~p~n", [Result]),
            Result
    end.

check_authorize(Authorization, StateData) -> 
    case StateData#state.channel_id of
        <<"viber">> -> true;
        <<"facebook">> -> true;
        <<"skype">> -> true;
        _ ->
            case binary:split(Authorization, <<".">>, [global]) of
                [_H, P, _S] ->
                    AuthJson = jsx:decode(base64:decode(P)),
                    AUD = proplists:get_value(<<"aud">>, AuthJson),
                    ?DEBUG("#69 AuthJson: ~p, AUD: ~p~n", [AuthJson, AUD]),
                    AUD == StateData#state.ms_app_id;
                _ ->
                    false
            end
    end.

get_web_api(ChannelId, Webbase, BotConfig) ->
	case get_kite_app_id(ChannelId, BotConfig) of
		<<>> ->
			"";
		KiteAppId ->
			Webbase ++ binary_to_list(KiteAppId) ++ "/request-support"
	end.

get_kite_app_id(<<"facebook">>, BotConfig) ->
	case get(<<"kite_app_id_for_facebook">>, BotConfig) of
		<<>> ->
			get(<<"kite_app_id">>, BotConfig);
		Result ->
			Result
	end;

get_kite_app_id(<<"skype">>, BotConfig) ->
	get(<<"kite_app_id_for_skype">>, BotConfig);

get_kite_app_id(<<"viber">>, BotConfig) ->
    get(<<"kite_app_id">>, BotConfig);

get_kite_app_id(_, BotConfig) ->
	<<>>. 

get_social_info(<<"facebook">>, #state{fb_token= FBToken
								, user_account= UserAccount}) ->
	SenderId = proplists:get_value(<<"id">>, UserAccount),
	get_facebook_info(SenderId, FBToken);

get_social_info(<<"viber">>, #state{user_account = UserAccount,
                                    bot_account = BotAccount}) ->
    SenderId = get(<<"id">>, UserAccount),
    AuthToken = get(<<"auth_token">>, BotAccount),
    get_viber_info(SenderId, AuthToken);

get_social_info(_ChannelId, _State) ->
	[].

get_facebook_info(undefined, _FBToken) ->
   [];  
get_facebook_info(SenderId, FBToken) when is_binary(SenderId) ->
    get_facebook_info(binary_to_list(SenderId), FBToken);
get_facebook_info(SenderId, FBToken) when is_binary(FBToken) ->
	get_facebook_info(SenderId, binary_to_list(FBToken)); 
get_facebook_info(SenderId, FBToken) ->
	Url = ?FACEBOOK_API ++ SenderId ++ "?fields=first_name,last_name,locale,timezone,gender,profile_pic&access_token=" ++ FBToken,
	case mod_social_util:request_http_get(Url, []) of
		Result when is_list(Result) -> 
			[{<<"senderId">>, list_to_binary(SenderId)} |Result];
		_ -> []
	end.


get_facebook_info_with_msgid(MsgId, FBToken) when is_binary(MsgId) ->
    get_facebook_info_with_msgid(binary_to_list(MsgId), FBToken);
get_facebook_info_with_msgid(MsgId, FBToken) when is_binary(FBToken) ->
    get_facebook_info_with_msgid(MsgId, binary_to_list(FBToken));
get_facebook_info_with_msgid(MsgId, FBToken) ->
    Url = ?FACEBOOK_API ++ "m_" ++ MsgId ++ "?fields=from&access_token=" ++ FBToken,
    ?INFO_MSG("mod_social_c2s: facebook_with_msgid: ~p",[Url]),
    case mod_social_util:request_http_get(Url, []) of
        Result when is_list(Result) -> 
            FromInfo = proplists:get_value(<<"from">>, Result, []),
            proplists:get_value(<<"name">>, FromInfo, <<>>);
        _ -> <<>>
    end.

get_viber_info(undefined, _AuthenToken) ->
    [];

get_viber_info(SenderId, AuthToken) ->
    Data = [{<<"id">>, SenderId}],
    Res = httpc:request(post, {?GET_USER_DETAIL, [{"X-Viber-Auth-Token", binary_to_list(AuthToken)}], 
           ?CONTENT_TYPE_JSON, jsx:encode(Data)}, [], []),
    ?INFO_MSG("get_viber_info: ~p~n", [Res]),
    proplists:get_value(<<"user">>, Res).


get_publicaccount_info() ->
    {_,{_, _, Res}} = httpc:request(post, {"https://chatapi.viber.com/pa/get_account_info", [{"X-Viber-Auth-Token", "4570685a8834974f-c44db3ce874682f9-a427f6bdc2a890aa"}], 
           ?CONTENT_TYPE_JSON, jsx:encode([{}])}, [], []),
    ?INFO_MSG("get_viber_info: ~p~n", [Res]),
    proplists:get_value(members, Res).
    %jsx:decode(Res).


get_failover_username(UserAccount, SocialInfo, MsgId, FbToken) ->
    ?INFO_MSG("mod_social_c2s:  UserAccount: ~p",[UserAccount]),
    ?INFO_MSG("mod_social_c2s: SocialInfo: ~p", [SocialInfo]),
    UserName = get(<<"name">>, UserAccount),
    FirstName = proplists:get_value(<<"first_name">>, SocialInfo, <<>>),
        LastName = proplists:get_value(<<"last_name">>, SocialInfo, <<>>),
    if UserName /= <<"">>, UserName /= <<>>, UserName /= "", UserName /= <<" ">> ->
        UserName ;
    (FirstName /= <<>> andalso FirstName /= <<" ">>)
        orelse (LastName /= <<>> andalso LastName /= <<" ">>)  ->
        <<FirstName/binary, " ", LastName/binary>>;
    true -> 
        get_facebook_info_with_msgid(MsgId, FbToken)
    end. 

