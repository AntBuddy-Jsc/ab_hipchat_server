-module(mod_beeiq_handler).

-include("mod_social.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mam.hrl").
-include("mod_antbuddy_stanza.hrl").

-export([init/3, terminate/3]).
-export([rest_init/2]).
-export([known_methods/2
        , allowed_methods/2
        , content_types_accepted/2
        , content_types_provided/2
        , resource_exists/2
        , delete_completed/2
        , delete_resource/2
        , is_authorized/2]).
-export([to_json/2]).
-export([handle_from_json/2]).
-export([process_kite_message/6, process_event_message/5]).
-export([get_beeiq_app_info/1, authen_hook_url/2, request_url/4, catch_posting_message/2]).
-export([item_to_binary/2]).

-record(beeiq_state, {
        web_base     :: string(),
        kite_identity :: string()
    }).

init({tcp, http}, _Req, _) ->
    ?INFO_MSG("1. init: ~n", []),
    {upgrade, protocol, cowboy_rest};
init(_Type, _Req, _) ->
    ?INFO_MSG("2. init: ~n", []),
    {upgrade, protocol, cowboy_rest}.

rest_init(Req0, [_Host, Opts]) ->
    WebUrl = proplists:get_value(web_api, Opts),
    KiteIdentity = proplists:get_value(kite_identity, Opts),
    ?INFO_MSG("3. rest_init: ~p~n", [WebUrl]),
    {ok, Req0, #beeiq_state{web_base = WebUrl, kite_identity = KiteIdentity}}.

rest_terminate(_Req, _State) ->
    ok.

terminate(_Reason, _Req, _State) ->
    ok.

%%%======================================
%%% Cowboy HTTP Rest API Callbacks
%%% GET     /bots
%%% GET     /bots/id
%%% POTS    /bots
%%% PUT     /bots/id
%%% DELETE  /bots/id
%%%======================================
known_methods(Req, State) ->
    ?DEBUG("run: known_methods ~n", []),
    {?ALLOWED_METHODS, Req, State}.

allowed_methods(Req0, State) ->
    ?DEBUG("run: allowed_methods~n", []),
    AllowedMethods = case cowboy_req:bindings(Req0) of
        {[], _} ->
            [?HTTP_GET, ?HTTP_POST];
        {[{beeiq_id, _}], _} ->
                [?HTTP_GET, ?HTTP_POST, ?HTTP_PUT, ?HTTP_DELETE];
        _ ->
            []
    end,
    {AllowedMethods, Req0, State}.

%% Methods: POST, PUT, PATCH
content_types_accepted(Req, State) ->
    ?DEBUG("run: content_types_accepted Req: ~p~n", [Req]),
    {[{{<<"application">>, <<"json">>, []}, handle_from_json}],
        Req, State}.

%% Methods: GET, HEAD
content_types_provided(Req, State) ->
    ?DEBUG("run: content_types_provided Req: ~p~n", [Req]),
    {[
        {{<<"application">>, <<"json">>, '*'}, to_json}
    ], Req, State}.

%% Methods: DELETE
delete_resource(Req, State) ->
    ?DEBUG("run: delete_resource~n", []),
    {Binding, Req2} = cowboy_req:binding(beeiq_id, Req),
    Res = delete_beeiq_app(Req2, State, Binding),
    {Res, Req, State}.

%% Methods: DELETE
delete_completed(Req, State) ->
    ?DEBUG("run: delete_completed~n", []),
    {true, Req, State}.

resource_exists(Req, State) ->
    ?DEBUG("run: resource_exists Req: ~p~n", [Req]),
    {AllBindings, _} = cowboy_req:bindings(Req),
    IsExist = case AllBindings of
        [] ->
            true;
        [{beeiq_id, _}] ->
            true;
        _ ->
            false
    end,
    {IsExist, Req, State}.

%%% echo -n "Aladdin:OpenSesame" | base64
is_authorized(Req, State) ->
    ?DEBUG("run: is_authorized ~n", []),
    {Binding, _} = cowboy_req:binding(beeiq_id, Req),
    if Binding == <<"beeiq_app">> ->
        {true, Req, State};
       Binding == <<"send_message">> ->
        {true, Req, State}; 
      true ->

        % case cowboy_req:parse_header(<<"authorization">>, Req) of
        %     {ok, {Token, _}, _} ->
        %         case check_auth_token(Token) of
        %             true ->
        %                 delete_auth_token(Token),
        %                 {true, Req, State};
        %             _ ->
        %                 {{false, <<"Wrong token">>}, Req, State}
        %         end;
        %     _ ->
        %         {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
        % end
        %{{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
        {true, Req, State}
    end.
     

to_json(Req, State) ->
    ?DEBUG("to_json~n", []),
    case handle_request(Req, State) of
        [] ->
            {ok, Req2} = cowboy_req:reply(404, [{<<"content-type">>, <<"application/json">>}], 
                                            [], Req),
            {halt, Req2, State};
        Body ->
            ?INFO_MSG("Body: ~p~n", [Body]),
            {jsx:encode(Body), Req, State}
    end.

% {"bot_app_id": "Antbuddy", 
% "ms_app_id": "hdjksahdksahdjsajkdsajkd", 
% "ms_app_secret": "nsadsadksa", 
% "fb_token": "nsadsadksa", 
% "kite_app_id": "dsadksaldka",			// kite_app_id_for_facebook
% "kite_app_id_for_skype": "dsadksaldka",}
handle_from_json(Req, State) ->
    ?INFO_MSG("handle_from_json: Req: ~p~n", [Req]),
    {Binding, _} = cowboy_req:binding(beeiq_id, Req),
    {ok, PostVals, Req2} = cowboy_req:body(Req),
    {Method, Req3} = cowboy_req:method(Req2),
    ?INFO_MSG("handle_from_json: Binding: ~p~n", [Binding]),
    if 
       Binding == <<"send_message">> -> 
        handle_beeiq_send_message(Method, PostVals, Req3, State);
      true ->
        handle_beeiq_app(Method, PostVals, Req3, State)
    end.

handle_request(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {AllBindings, Req3} = cowboy_req:bindings(Req2),
    case AllBindings of
        [{beeiq_id, KiteAppId}] ->
            validate_method(Method, Req3, State, KiteAppId);
        _ ->
            [{<<"result">>, <<"Wrong request">>}]
    end.

validate_method(?HTTP_GET, _Req, _State, KiteAppId) ->
    get_beeiq_app_info([{<<"kite_app_id">>, KiteAppId}]);
validate_method(_, _, _, _) ->
    [].

handle_beeiq_send_message(?HTTP_POST, BodyVals, Req, State) ->
	?INFO_MSG("handle_beeiq_send_message: ~ts~n", [BodyVals]),
    case BodyVals of
        [{BodyReq, true}] ->
            send_message(jsx:decode(BodyReq), Req, State);
        [{true, BodyReq}] ->
            send_message(jsx:decode(BodyReq), Req, State);
        BodyReq when BodyReq /= <<"{}">>->
            send_message(jsx:decode(BodyReq), Req, State);
        _ ->            
            {true, Req, State}
    end;
handle_beeiq_send_message(_, _, Req, State) ->
    {true, Req, State}.

handle_beeiq_app(?HTTP_POST, BodyVals, Req, State) ->
    ?INFO_MSG("handle_beeiq_app: ~p~n", [BodyVals]),
    case BodyVals of
        [{BodyReq, true}] ->
            create_beeiq_app(jsx:decode(BodyReq), Req, State);
        [{true, BodyReq}] ->
            create_beeiq_app(jsx:decode(BodyReq), Req, State);
        BodyReq when BodyReq /= <<"{}">> ->
            create_beeiq_app(jsx:decode(BodyReq), Req, State);
        _ ->
            {true, Req, State}
    end;
handle_beeiq_app(?HTTP_PUT, BodyVals, Req, State) ->
    {Binding, Req2} = cowboy_req:binding(beeiq_id, Req),
    ?INFO_MSG("Binding: ~p~n", [Binding]),
    case BodyVals of
        [{BodyReq, true}] ->
            update_beeiq_app(Binding, jsx:decode(BodyReq), Req2, State);
        [{true, BodyReq}] ->
            update_beeiq_app(Binding, jsx:decode(BodyReq), Req2, State);
        BodyReq ->
            update_beeiq_app(Binding, jsx:decode(BodyReq), Req2, State);
        _ ->
            {true, Req2, State}
    end;
handle_beeiq_app(_, _, Req, State) ->
    {true, Req, State}.

send_message(BodyReq, Req, State) -> 
	?INFO_MSG("BodyReq: ~p~n", [BodyReq]),
    ?INFO_MSG("web_base: ~p~n", [State#beeiq_state.web_base]),
	FromInfo = proplists:get_value(<<"from">>, BodyReq, []),
	KiteAppId = item_to_binary(kite_app_id, proplists:get_value(<<"id">>, FromInfo, <<>>)),
    KiteAppName = item_to_binary(kite_app_name, proplists:get_value(<<"name">>, FromInfo, <<>>)),
    Avatar = item_to_binary(avatar, proplists:get_value(<<"avatar">>, FromInfo, <<>>)),
	ToInfo = proplists:get_value(<<"to">>, BodyReq, []),
	CustomerId = item_to_binary(customer_id, proplists:get_value(<<"id">>, ToInfo, <<>>)),
	MessageInfo = proplists:get_value(<<"message">>, BodyReq, []),
	MessageId = item_to_binary(message_id, proplists:get_value(<<"mid">>, MessageInfo, mod_antbuddy_stanza:item_to_binary(mod_mam_utils:generate_message_id()))),
	Text = item_to_binary(message_text, proplists:get_value(<<"text">>, MessageInfo, <<>>)),
    MsgType = item_to_binary(message_type, proplists:get_value(<<"type">>, MessageInfo, <<"supporter">>)),
    RoomId = item_to_binary(room_id, proplists:get_value(<<"room_id">>, MessageInfo, <<>>)),
    RoomHost = item_to_binary(room_host, proplists:get_value(<<"room_host">>, MessageInfo, <<>>)),
    EventType = item_to_binary(event_type, proplists:get_value(<<"event">>, MessageInfo, <<>>)),
    FileInfo = proplists:get_value(<<"file">>, MessageInfo, []),
    AccessToken =
    case cowboy_req:header(<<"x-auth-token">>, Req) of
        {Token, _} -> Token;
        Other -> Other
    end, 
    ?INFO_MSG("AccessToken: ~p; KiteAppId: ~p~n", [AccessToken, KiteAppId]),
    ?INFO_MSG("EventType: ~p; Events List: ~p~n", [EventType, ?KITE_EVENTS]),
    case check_auth_token(KiteAppId, AccessToken) of
        true ->
            case lists:member(EventType, ?KITE_EVENTS) of
                true ->
                    AbRequestMsgId = proplists:get_value(<<"ab_request_msg_id">>, MessageInfo, <<>>), 
                    send_event(AbRequestMsgId, EventType, KiteAppId, KiteAppName, Avatar, RoomId, MessageId, Req, BodyReq, State);
                _ ->
                    RoomJid = << RoomId/binary, "@", RoomHost/binary >>,
                    ?INFO_MSG("RoomJid: ~p~n", [RoomJid]),                	                		
                	send_message_to_room(CustomerId, RoomJid, MessageId, Text, MsgType, Req, BodyReq, KiteAppName, Avatar, FileInfo, State)               			
            end;
        _ ->
            {ok, Req2} = cowboy_req:reply(400, [{<<"content-type">>, <<"application/json">>}], 
                                                    jsx:encode([{<<"error">>, <<"Invalid auth token">>}]), Req),
            {halt, Req2, State}
    end.

check_auth_token(KiteAppId, AccessToken) ->
    case ets:lookup(track_beeiq_accesstoken, KiteAppId) of
        [{_, AccessToken}] -> 
            ?INFO_MSG("check_auth_token: AccessToken: ~p~n", [AccessToken]),
            true;
        _ ->
            BeeIQAppInfo = get_beeiq_app_info([{<<"kite_app_id">>, KiteAppId}]),
            case proplists:get_value(<<"access_token">>, BeeIQAppInfo) of
                AccessToken ->
                    ?INFO_MSG("check_auth_token: cache token: ~p~n", [AccessToken]),
                    ets:insert(track_beeiq_accesstoken, {KiteAppId, AccessToken}), 
                    true;
                _ ->
                    ?INFO_MSG("check_auth_token: token: ~p not existing ~n", [AccessToken]), 
                    false
            end
    end.


send_message_to_room(BCustomerId, BRoomJid, MessageId, Text, MsgType, Req, BodyReq, KiteAppName, Avatar, FileInfo, State) ->
	RoomJid = jlib:binary_to_jid(BRoomJid),
	{CustomerJid, KiteHost} = 
	case binary:split(RoomJid#jid.lserver, <<".">>) of
		[<<"conference">>, Host] ->
			{jlib:make_jid(BCustomerId, Host, <<>>), Host};
		_ ->
			{RoomJid, RoomJid#jid.lserver}
	end,
    ?INFO_MSG("send_message_to_room: CustomerJid: ~p~n", [CustomerJid]),
	BCustomerJid = jlib:jid_to_binary(CustomerJid),
	MsgStanza = make_xmpp_message(MessageId, BCustomerJid, BRoomJid, Text, MsgType, KiteAppName, Avatar, FileInfo),
    %?INFO_MSG("MsgStanza: ~p~n", [MsgStanza]),
	case ejabberd_sm:get_user_resources(BCustomerId, KiteHost) of
		Resources when Resources /= [] ->
			lists:foreach(fun(Res) ->
				CustomerJidWithResource = jlib:jid_replace_resource(CustomerJid, Res),
				ejabberd_router:route(CustomerJidWithResource, RoomJid, MsgStanza)
			end, Resources);
		_ ->
			ok
	end,	
	{ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], 
                     jsx:encode([{<<"result">>, BodyReq ++ [{<<"mid">>, MessageId}]}]), Req),
	{halt, Req2, State}.

send_event(AbRequestMsgId, EventType, _KiteAppId, KiteAppName, Avatar, RoomId, MessageId, Req, BodyReq, State) ->
    WebUrl = request_url(State#beeiq_state.web_base, EventType, RoomId, AbRequestMsgId),
    ?INFO_MSG("WebUrl: ~p; KiteIdentity: ~p~n", [WebUrl, State#beeiq_state.kite_identity]),
    Message = [{<<"key">>, RoomId}, {<<"supporter">>, [{<<"name">>, KiteAppName}, {<<"avatar">>, Avatar}]}
    , {<<"from">>, <<"BeeIQ">>}],

    ?INFO_MSG("send_event to kite web: Message: ~p~n", [Message]),
    Response = httpc:request(put, {WebUrl, [{"kite-identity", State#beeiq_state.kite_identity}, {"Content-Type", ?CONTENT_TYPE_JSON}], ?CONTENT_TYPE_JSON, jsx:encode(Message)}, [], []),
    ?INFO_MSG("Event Response: ~p~n", [Response]),
    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], 
                     jsx:encode([{<<"result">>, BodyReq ++ [{<<"mid">>, MessageId}]}]), Req),
    {halt, Req2, State}.  

request_url(WebApi, EventType, RoomId, AbRequestMsgId) ->
    BWebApi = list_to_binary(WebApi),
    case binary:split(BWebApi, <<"kites/">>) of
        [Api, _] ->
            case EventType of
                <<"kite-start">> -> 
                    binary_to_list(Api) ++ "rooms/"  ++ binary_to_list(AbRequestMsgId) ++ "/take-request-support";
                <<"kite-close">> ->
                    binary_to_list(Api) ++ "rooms/"  ++ binary_to_list(RoomId) ++ "/close-room";
                _ ->  
                    binary_to_list(Api)
            end;   
        [Other] -> binary_to_list(Other);
        _ -> []
    end.

make_xmpp_message(MsgId, BCustomerJid, BRoomJid, Content, MsgType, KiteAppName, Avatar, FileInfo) -> 
    KiteTag = #xmlel{name = <<"kite">>, children = [#xmlel{name = <<"type">>, children = [#xmlcdata{content = MsgType}]}
    , #xmlel{name = <<"kite_app_name">>, children = [#xmlcdata{content = KiteAppName}]}
    , #xmlel{name = <<"avatar">>, children = [#xmlcdata{content = Avatar}]}
    , #xmlel{name = <<"provider">>, children = [#xmlcdata{content = <<"BeeIQ">>}]}
    ]},
    Attrs = [{<<"type">>, <<"groupchat">>}, {<<"id">>, MsgId}, 
                {<<"from">>, BCustomerJid}, {<<"to">>, BRoomJid}],
    ?INFO_MSG("make_xmpp_message: FileInfo: ~p~n", [FileInfo]),
    if FileInfo == [] ->
        BodyEl = #xmlel{name = <<"body">>, children = [#xmlcdata{content = Content}]},
        #xmlel{name = <<"message">>, attrs=Attrs, children=[BodyEl, KiteTag]};
      true ->
        ThumbnailUrl = item_to_binary(thumbnailurl, proplists:get_value(?FILE_THUMBNAIL_URL, FileInfo, <<>>)),
        ThumbnailWidth = item_to_binary(thumbnailwith, proplists:get_value(?FILE_THUMBNAIL_WIDTH, FileInfo, <<>>)),
        ThumbnailHeight = item_to_binary(thumbnailheight, proplists:get_value(?FILE_THUMBNAIL_HEIGHT, FileInfo, <<>>)),
        MimeType = item_to_binary(mimetype, proplists:get_value(?MIMETYPE, FileInfo, <<>>)),
        FileUrl = item_to_binary(fileurl, proplists:get_value(?FILE_URL, FileInfo, <<>>)),
        Size = item_to_binary(filesize, proplists:get_value(?FILE_SIZE, FileInfo, <<>>)),
        Name = item_to_binary(filename, proplists:get_value(?FILE_NAME, FileInfo, <<>>)),   

        BodyText = << <<"File uploaded: ">>/binary, FileUrl/binary >>,
        BodyTag = #xmlel{name = <<"body">>, children = [#xmlcdata{content = BodyText}]},             
        FileTag = #xmlel{name = <<"file">>, children = [#xmlel{name = <<"thumbnailUrl">>, children = [#xmlcdata{content = ThumbnailUrl}]}
        , #xmlel{name = <<"thumbnailWidth">>, children = [#xmlcdata{content = ThumbnailWidth}]}
        , #xmlel{name = <<"thumbnailHeight">>, children = [#xmlcdata{content = ThumbnailHeight}]}
        , #xmlel{name = <<"mimeType">>, children = [#xmlcdata{content = MimeType}]}
        , #xmlel{name = <<"fileUrl">>, children = [#xmlcdata{content = FileUrl}]}
        , #xmlel{name = <<"size">>, children = [#xmlcdata{content = Size}]}
        , #xmlel{name = <<"name">>, children = [#xmlcdata{content = Name}]}
        ]},
        #xmlel{name = <<"message">>, attrs=Attrs, children=[BodyTag, KiteTag, FileTag]}
    end.

replace_modified_time(Record) ->
	case proplists:get_value(<<"modify_at">>, Record) of
		undefined ->
			Record;
		ModifiedTime when is_tuple(ModifiedTime) ->
			lists:keyreplace(<<"modify_at">>, 1, Record,
					{<<"modify_at">>, calendar:now_to_universal_time(ModifiedTime)});
        _ ->
            Record
	end.

create_beeiq_app(BodyReq, Req, State) ->
	NewBodyReq = BodyReq ++ [{<<"create_at">>, os:timestamp()}, {<<"modify_at">>, os:timestamp()}],
    ?DEBUG("BodyReq: ~p~n", [BodyReq]),
    BeeIQAppId = proplists:get_value(<<"kite_app_id">>, BodyReq),
    HookUrl = proplists:get_value(<<"kite_app_hook_url">>, NewBodyReq),
    case authen_hook_url(BeeIQAppId, HookUrl) of
        true ->
            Chk = check_existing_beeiq_app(BeeIQAppId),
            case Chk of
                false ->
                    case mod_mongo:create_beeiq_app(BodyReq) of
                        ok ->
                            AccessToken = proplists:get_value(<<"access_token">>, NewBodyReq),
                            ets:insert(track_beeiq_accesstoken, {BeeIQAppId, AccessToken}),
                            ets:insert(track_beeiq_hookurl, {BeeIQAppId, HookUrl}),
                            {ok, Req2} = cowboy_req:reply(201, [{<<"content-type">>, <<"application/json">>}], 
                                    jsx:encode([{<<"result">>, BodyReq ++ [{<<"Location">>, BeeIQAppId}]}]), Req),
                            {halt, Req2, State};
                        _ ->
                            {true, Req, State}
                    end;
                _ -> 
                    {ok, Req3} = cowboy_req:reply(409, [{<<"content-type">>, <<"application/json">>}], 
                                                    jsx:encode([{<<"error">>, <<"kite_app_id already existed">>}]), Req),
                    {halt, Req3, State}
            end;
        _ ->
            {ok, Req4} = cowboy_req:reply(400, [{<<"content-type">>, <<"application/json">>}], 
                                                    jsx:encode([{<<"error">>, <<"Wrong kite_app_id or code">>}]), Req),
            {halt, Req4, State}
    end.

get_beeiq_app_info(Selector) ->
    Res = case mod_mongo:fetch_beeiq_app(Selector) of
        [] ->
            [];
        BeeIQInfo ->
            Record = lists:last(BeeIQInfo),
			replace_modified_time(Record)
    end,
    lists:keydelete(<<"_id">>, 1, Res).

update_beeiq_app(KiteAppId, Body, Req, State) ->
	NewBody = 
    case proplists:get_value(<<"modify_at">>, Body) of
        undefined -> 
            Body ++ [{<<"modify_at">>, os:timestamp()}];
        _ -> 
            Body
    end,
    HookUrl = proplists:get_value(<<"kite_app_hook_url">>, NewBody),
    case authen_hook_url(KiteAppId, HookUrl) of
        true ->
            Chk = check_existing_beeiq_app(KiteAppId),
            case Chk of
                true ->
                    case mod_mongo:update_beeiq_app([{<<"kite_app_id">>, KiteAppId}],
                                                                            NewBody) of
                        ok ->
                            ets:insert(track_beeiq_hookurl, {KiteAppId, HookUrl}),
                            {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], 
                                                    jsx:encode([{<<"result">>, Body}]), Req),
                            {halt, Req2, State};
                        _ ->
                            {true, Req, State}
                    end;
                _ ->
                    {ok, Req3} = cowboy_req:reply(404, [{<<"content-type">>, <<"application/json">>}], 
                                                    jsx:encode([{<<"error">>, <<"kite_app_id not exists">>}]), Req),
                    {halt, Req3, State}
            end;
        _ ->
            {ok, Req4} = cowboy_req:reply(400, [{<<"content-type">>, <<"application/json">>}], 
                                                    jsx:encode([{<<"error">>, <<"Wrong kite_app_id or code">>}]), Req),
            {halt, Req4, State}
    end. 

delete_beeiq_app(Req, State, KiteAppId) ->
    Selector = [{<<"kite_app_id">>, KiteAppId}],
    case mod_mongo:fetch_beeiq_app(Selector) of
        [] ->
            {ok, Req2} = cowboy_req:reply(404, 
                                    [{<<"content-type">>, <<"application/json">>}], 
                                    [], Req),
            {halt, Req2, State};
        _ ->
            case mod_mongo:delete_beeiq_app(Selector) of
                ok ->
                    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], 
                                    jsx:encode([{<<"result">>, <<"Deleted ", KiteAppId/binary>>}]), Req),
                    {halt, Req2, State};
                _ ->
                    false
            end
    end.  

process_event_message(EventType, MsgId, DateTime, RoomJid, KiteTag) ->
    SenderId = xml:get_subtag_cdata(KiteTag, <<"id">>),
    SenderName = xml:get_subtag_cdata(KiteTag, <<"name">>),
    SenderAvatar = xml:get_subtag_cdata(KiteTag, <<"avatar">>),
    KiteAppId = xml:get_subtag_cdata(KiteTag, <<"kite_app_id">>),
    KiteAppName = xml:get_subtag_cdata(KiteTag, <<"kite_app_name">>),
    Timestamp = timestamp_milisec(DateTime),
    MsgType = xml:get_subtag_cdata(KiteTag, <<"type">>),
    AbRequestMsgId = xml:get_subtag_cdata(KiteTag, <<"ab_request_msg_id">>),
    Phone = xml:get_subtag_cdata(KiteTag, <<"phone">>),

    BeeIQHookUrl = 
    case ets:lookup(track_beeiq_hookurl, KiteAppId) of
        [{_, HookUrl}] -> HookUrl;
        _ ->
            BeeIQAppInfo = get_beeiq_app_info([{<<"kite_app_id">>, KiteAppId}]),
            case proplists:get_value(<<"kite_app_hook_url">>, BeeIQAppInfo) of
                undefined -> <<>>;
                Url when Url /= <<>> -> 
                    ets:insert(track_beeiq_hookurl, {KiteAppId, Url}),
                    Url;
                _ ->
                    <<>>
            end
    end,

    Message = [{<<"from">>, [{<<"id">>, SenderId}, {<<"name">>, 
     SenderName}, {<<"avatar">>, SenderAvatar}]}
     , {<<"to">>, [{<<"id">>, KiteAppId}, {<<"name">>, KiteAppName}]}
     , {<<"message">>, [{<<"mid">>, MsgId}, {<<"room_id">>, RoomJid#jid.luser}, {<<"room_host">>, RoomJid#jid.lserver}
     , {<<"event">>, EventType}, {<<"type">>, MsgType}, {<<"ab_request_msg_id">>, AbRequestMsgId}, {<<"phone">>, Phone}]}, {<<"timestamp">>, Timestamp}], 

    send_message_to_beeiq(BeeIQHookUrl, Message).

process_kite_message(Packet, BodyMsg, MsgId, DateTime, RoomJid, KiteTag) ->
    ?INFO_MSG("send_message_to_beeiq: KiteTag: ~p~n", [KiteTag]),
	SenderId = xml:get_subtag_cdata(KiteTag, <<"id">>),
	SenderName = xml:get_subtag_cdata(KiteTag, <<"name">>),
	SenderAvatar = xml:get_subtag_cdata(KiteTag, <<"avatar">>),
	KiteAppId = xml:get_subtag_cdata(KiteTag, <<"kite_app_id">>),
	KiteAppName = xml:get_subtag_cdata(KiteTag, <<"kite_app_name">>),
	Timestamp = timestamp_milisec(DateTime),
    MsgType = xml:get_subtag_cdata(KiteTag, <<"type">>),
    AbRequestMsgId = xml:get_subtag_cdata(KiteTag, <<"ab_request_msg_id">>),
    Phone = xml:get_subtag_cdata(KiteTag, <<"phone">>),
    Language = xml:get_subtag_cdata(KiteTag, <<"language">>),
    SubType = xml:get_tag_attr_s(<<"subtype">>, Packet),

	BeeIQHookUrl = 
	case ets:lookup(track_beeiq_hookurl, KiteAppId) of
		[{_, HookUrl}] -> HookUrl;
		_ ->
			BeeIQAppInfo = get_beeiq_app_info([{<<"kite_app_id">>, KiteAppId}]),
			case proplists:get_value(<<"kite_app_hook_url">>, BeeIQAppInfo) of
				undefined -> <<>>;
				Url when Url /= <<>> -> 
					ets:insert(track_beeiq_hookurl, {KiteAppId, Url}),
					Url;
				_ ->
					<<>>
			end
	end,

	Message0 = [{<<"from">>, [{<<"id">>, SenderId}, {<<"name">>, 
	 SenderName}, {<<"avatar">>, SenderAvatar}]}
	 , {<<"to">>, [{<<"id">>, KiteAppId}, {<<"name">>, KiteAppName}]}
	 , {<<"message">>, [{<<"mid">>, MsgId}, {<<"text">>, BodyMsg}, {<<"room_id">>, RoomJid#jid.luser}, {<<"room_host">>, RoomJid#jid.lserver}
     , {<<"type">>, MsgType}, {<<"ab_request_msg_id">>, AbRequestMsgId}, {<<"language">>, Language}, {<<"subtype">>, SubType}, {<<"phone">>, Phone}]}
     , {<<"timestamp">>, Timestamp}], 

    FileInfo = 
    case xml:get_subtag(Packet, <<"file">>) of
        false -> [];
        FileTag ->
            %?INFO_MSG("FileTag: ~p~n", [FileTag]),
            ThumbnailUrl = xml:get_subtag_cdata(FileTag, ?FILE_THUMBNAIL_URL),
            ThumbnailWidth = xml:get_subtag_cdata(FileTag, ?FILE_THUMBNAIL_WIDTH),
            ThumbnailHeight = xml:get_subtag_cdata(FileTag, ?FILE_THUMBNAIL_HEIGHT),
            MimeType = xml:get_subtag_cdata(FileTag, ?MIMETYPE),
            FileUrl = xml:get_subtag_cdata(FileTag, ?FILE_URL),
            Size = xml:get_subtag_cdata(FileTag, ?FILE_SIZE),
            Name = xml:get_subtag_cdata(FileTag, ?FILE_NAME),
            [{<<"file">>, [{<<"thumbnailUrl">>, ThumbnailUrl}, {<<"thumbnailWidth">>, ThumbnailWidth}, {<<"thumbnailHeight">>, ThumbnailHeight}
            , {<<"mimeType">>, MimeType}, {<<"fileUrl">>, FileUrl}, {<<"size">>, Size}, {<<"name">>, Name}]}]
    end,
    ?INFO_MSG("FileInfo: ~p~n", [FileInfo]),
	case xml:get_subtag(Packet, <<"customer_info">>) of
		false ->
			send_message_to_beeiq(BeeIQHookUrl, Message0 ++ FileInfo);
		CustomerTag ->
			BRoomJid = jlib:jid_to_binary(jlib:jid_remove_resource(RoomJid)),
			ets:insert(track_beeiq_customer_room, {SenderId, BRoomJid}),
			ets:insert(track_beeiq_room_kiteappid, {BRoomJid, KiteAppId}),
			send_message_to_beeiq(BeeIQHookUrl, Message0 ++ FileInfo, CustomerTag)
	end.	

send_message_to_beeiq(BeeIQHookUrl, Message0) ->
    ?INFO_MSG("send_message_to_beeiq: supporter: BeeIQHookUrl: ~p; Message: ~p~n", [BeeIQHookUrl, Message0]),
    catch_posting_message(BeeIQHookUrl, Message0).

	% Response = httpc:request(post, {binary_to_list(BeeIQHookUrl), [{"Content-Type", ?CONTENT_TYPE_JSON}], 
 %        ?CONTENT_TYPE_JSON, jsx:encode(Message0)}, [], []),
 %    ?INFO_MSG("Reponse: ~p~n", [Response]).

send_message_to_beeiq(BeeIQHookUrl, Message0, CustomerTag) -> 
	CurrentView = xml:get_subtag_cdata(CustomerTag, <<"current_view">>),
	Email = xml:get_subtag_cdata(CustomerTag, <<"email">>),
	Phone = xml:get_subtag_cdata(CustomerTag, <<"phone">>),
	Preferer = xml:get_subtag_cdata(CustomerTag, <<"preferer">>),
	Gender = xml:get_subtag_cdata(CustomerTag, <<"gender">>),
	Status = xml:get_subtag_cdata(CustomerTag, <<"status">>),
	Location = xml:get_subtag_cdata(CustomerTag, <<"location">>),
	Browser = xml:get_subtag_cdata(CustomerTag, <<"browser">>),
	Os = xml:get_subtag_cdata(CustomerTag, <<"Os">>),
    NewSession = xml:get_subtag_cdata(CustomerTag, <<"new_session">>),
	CustomerInfo =
	[{<<"customer_info">>, [{<<"current_view">>, CurrentView}
	, {<<"email">>, Email}, {<<"phone">>, Phone}, {<<"preferer">>, Preferer}
	, {<<"gender">>, Gender}, {<<"status">>, Status}, {<<"location">>, Location}
	, {<<"browser">>, Browser}, {<<"Os">>, Os}, {<<"new_session">>, NewSession}]}],
	Message = Message0 ++ CustomerInfo,
    ?INFO_MSG("send_message_to_beeiq: Customer: BeeIQHookUrl: ~p; Message: ~p~n", [BeeIQHookUrl, Message]),
    catch_posting_message(BeeIQHookUrl, Message).

	% Response = httpc:request(post, {binary_to_list(BeeIQHookUrl), [{"Content-Type", ?CONTENT_TYPE_JSON}], 
 %        ?CONTENT_TYPE_JSON, jsx:encode(Message)}, [], []),
 %    ?INFO_MSG("Reponse: ~p~n", [Response]).

 catch_posting_message(BeeIQHookUrl, Message) ->
    try httpc:request(post, {binary_to_list(BeeIQHookUrl), [{"Content-Type", ?CONTENT_TYPE_JSON}],  
            ?CONTENT_TYPE_JSON, jsx:encode(Message)}, [], []) of
        {ok, {{_, 200, _}, _, Result}} ->
            ?INFO_MSG("Reponse Result: ~p~n", [Result]),
            case catch jsx:decode(list_to_binary(Result)) of
                {'EXIT', _Msg} -> 
                    ?ERROR_MSG("send_message_to_beeiq: Cannot decode response: ~p~n", [Result]),
                    error;
                Content -> 
                    {ok, Content}
            end;            
        Err ->
            ?ERROR_MSG("send_message_to_beeiq fail, error: ~p~n", [Err]),
            error
    catch
        _Exception:Reason -> 
            ?ERROR_MSG("send_message_to_beeiq: Reason: ~p~n", [Reason]),
            error
    end.

check_existing_beeiq_app(BeeIQAppId) -> 
    case get_beeiq_app_info([{<<"kite_app_id">>, BeeIQAppId}]) of
        [] -> false;
        _ -> true
    end.

authen_hook_url(KiteAppId, HookUrl) ->
    Code = mod_social_util:generate_resource(KiteAppId),
    ReqUrl = binary_to_list(HookUrl) ++ "?app_id=" ++ binary_to_list(KiteAppId) ++ "&code=" ++ binary_to_list(Code),
    ?INFO_MSG("authen_hook_url: ~p~n", [ReqUrl]),
    try httpc:request(get, {ReqUrl, []}, [{timeout, ?TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, Result}} ->
            ?INFO_MSG("authen_hook_url: Result: ~p~n", [Result]),
            case catch jsx:decode(list_to_binary(Result)) of
                {'EXIT', Msg} -> 
                    ?ERROR_MSG("Cannot decode response: ~p, reason: ~p~n", [Result, Msg]),
                    false;
                Content ->
                    ?INFO_MSG("authen_hook_url: Content: ~p~n", [Content]),
                    case {proplists:get_value(<<"app_id">>, Content), proplists:get_value(<<"code">>, Content)} of
                        {KiteAppId, Code} -> true;
                        _ -> false
                    end
            end;       
        Err ->
            ?ERROR_MSG("Error: ~p~n", [Err]),
            false
    catch
        _Exception:Reason -> 
            ?ERROR_MSG("Reason: ~p~n", [Reason]),
            false
    end. 

timestamp_milisec({Mega, Secs, Micro}) ->
	trunc((Mega * 1000000 * 1000000 + Secs * 1000000 + Micro)/1000).

item_to_binary(_FieldName, Item) when is_binary(Item)->
    Item;

item_to_binary(FieldName, Item) when is_integer(Item)->
    ?ERROR_MSG("BeeIQ Integer data: FieldName: ~p; Value: ~p~n", [FieldName, Item]),
    integer_to_binary(Item);

item_to_binary(FieldName, Item) when is_list(Item) ->
    ?ERROR_MSG("BeeIQ List data: FieldName: ~p; Value: ~p~n", [FieldName, Item]),
    list_to_binary(Item);

item_to_binary(FieldName, Item) when is_float(Item) ->
    ?ERROR_MSG("BeeIQ Float data: FieldName: ~p; Value: ~p~n", [FieldName, Item]),
    float_to_binary(Item);

item_to_binary(FieldName, Item) when is_atom(Item) ->
    ?ERROR_MSG("BeeIQ Atom data: FieldName: ~p; Value: ~p~n", [FieldName, Item]),
    atom_to_binary(Item, utf8);

item_to_binary(FieldName, Item) ->
    ?ERROR_MSG("BeeIQ Wrong Type data: FieldName: ~p; Value: ~p~n", [FieldName, Item]),
    Item.