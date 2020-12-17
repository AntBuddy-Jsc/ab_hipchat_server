-module(mod_social_util).

-export([ make_xmpp_message/4
        , make_reply_message/5
        , send_reply_message/4
        , sha1/1
        , make_presence_join_room/2
        , generate_resource/1
        , proc_name/2
        , get_ms_timestamp/0
        , is_expired_time/2
        , request_http_get/2
		, get_attachment_from_skype/3
        , get_attachment_from_viber/3
		, make_attachment_xmpp_message/4
        , make_reply_and_send_viber_message/5
        , make_viber_attachment_xmpp_message/4
		]).

-export([generate_token/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_social.hrl").

-spec make_xmpp_message(MsgId :: binary(), From :: ejabberd:jid(),
                    To :: ejabberd:jid(), Content :: binary() ) -> jlib:xmlel().
make_xmpp_message(MsgId, From, To, Content) -> 
    FromJID = jlib:jid_to_binary(From),
    ToJID = jlib:jid_to_binary(To),

    BodyEl = #xmlel{name = <<"body">>, children = [#xmlcdata{content = Content}]},
    Attrs = [{<<"type">>, <<"groupchat">>}, {<<"id">>, MsgId}, 
                {<<"from">>, FromJID}, {<<"to">>, ToJID}],
    #xmlel{name = <<"message">>, attrs=Attrs, children=[BodyEl]}.                

make_attachment_xmpp_message(MsgId, From, To, FileInfo) ->
	Get = fun proplists:get_value/2,
    FromJID = jlib:jid_to_binary(From),
    ToJID = jlib:jid_to_binary(To),

    BodyEl = make_sub_element(<<"body">>, Get(<<"fileUrl">>, FileInfo)),
	FileEl = #xmlel{name = <<"file">>
				, children = make_sub_element(<<"name">>, Get(<<"name">>, FileInfo))
							++ make_sub_element(<<"size">>, Get(<<"size">>, FileInfo))
							++ make_sub_element(<<"fileUrl">>, Get(<<"fileUrl">>, FileInfo))
							++ make_sub_element(<<"mimeType">>, Get(<<"mimeType">>, FileInfo))
							++ make_sub_element(<<"thumbnailUrl">>, Get(<<"thumbnailUrl">>, FileInfo))
							++ make_sub_element(<<"thumbnailWidth">>, Get(<<"thumbnailWidth">>, FileInfo))
							++ make_sub_element(<<"thumbnailWidth">>, Get(<<"thumbnailWidth">>, FileInfo))
							},
    Attrs = [{<<"type">>, <<"groupchat">>},
				{<<"id">>, MsgId},
                {<<"from">>, FromJID}, 
				{<<"to">>, ToJID}],
    #xmlel{name = <<"message">>, attrs=Attrs, children= [FileEl | BodyEl]}.

make_viber_attachment_xmpp_message(MsgId, From, To, FileInfo) ->
    Get = fun proplists:get_value/2,
    FromJID = jlib:jid_to_binary(From),
    ToJID = jlib:jid_to_binary(To),

    BodyEl = make_sub_element(<<"body">>, Get(<<"url">>, FileInfo)),
    ?INFO_MSG("BodyEl: ~p~n", [BodyEl]),
    FileEl = #xmlel{name = <<"file">>
                , children = make_sub_element(<<"name">>, Get(<<"name">>, FileInfo))
                            ++ make_sub_element(<<"size">>, Get(<<"size">>, FileInfo))
                            ++ make_sub_element(<<"fileUrl">>, Get(<<"url">>, FileInfo))
                            ++ make_sub_element(<<"mimeType">>, Get(<<"mimeType">>, FileInfo))
                            ++ make_sub_element(<<"thumbnailUrl">>, Get(<<"thumbnailUrl">>, FileInfo))
                            ++ make_sub_element(<<"thumbnailWidth">>, Get(<<"thumbnailWidth">>, FileInfo))
                            ++ make_sub_element(<<"thumbnailWidth">>, Get(<<"thumbnailWidth">>, FileInfo))
                            },

    Attrs = [{<<"type">>, <<"groupchat">>},
                {<<"id">>, MsgId},
                {<<"from">>, FromJID}, 
                {<<"to">>, ToJID}],
    #xmlel{name = <<"message">>, attrs=Attrs, children= [FileEl | BodyEl]}.


make_sub_element(Key, undefined) ->
	[];
make_sub_element(Key, Value) when is_list(Value) ->
	make_sub_element(Key, list_to_binary(Value));
make_sub_element(Key, Value) when is_integer(Value) ->
	make_sub_element(Key, list_to_binary(integer_to_list(Value)));
make_sub_element(Key, Value) when is_binary(Value) ->
	[#xmlel{name = Key, 
			children = [#xmlcdata{content = Value}]}].
    
-spec make_reply_message(ChannelId :: binary(), BotAccount :: list(), UserAccount :: list(),
                            Conversation :: list(), Reply :: reply()) -> binary().    
make_reply_message(ChannelId, BotAccount, UserAccount, Conversation, Reply) ->
    Message0 = [
                {<<"type">>, <<"message">>},
                {<<"from">>, BotAccount},
                {<<"recipient">>, UserAccount},
                {<<"conversation">>, Conversation}
            ],
    Message = case Reply#reply.attachments of 
        [] ->
			make_text_reply_message(ChannelId, Reply, Message0), 
            Message0 ++ [{<<"text">>, Reply#reply.text}];
        _ ->
			make_attachment_reply_message(ChannelId, Reply, Message0)
        end,    
    jsx:encode(Message).

make_reply_and_send_viber_message(Supporter, UserAccount, BotAccount, Content, Attachments) ->
    Get = fun proplists:get_value/2,
    Message0 = [{<<"receiver">>, proplists:get_value(<<"id">>, UserAccount)}
                , {<<"sender">>, [{<<"name">>, Get(<<"name">>, BotAccount)}, {<<"id">>, Supporter}]}
                %, {<<"type">>, <<"text">>}
                , {<<"tracking_data">>, <<"tracking data">>}],
                %, {<<"text">>, Content}],
    AuthenToken = proplists:get_value(<<"auth_token">>, BotAccount),
    if Content == <<>> -> no_reply;
        true ->
                %, {<<"type">>, <<"text">>}
            TextMsg = Message0 ++ [{<<"type">>, <<"text">>}, {<<"text">>, Content}],
            httpc:request(post, {?SEND_VIBER_MSG, [{"X-Viber-Auth-Token", binary_to_list(AuthenToken)}], 
            ?CONTENT_TYPE_JSON, jsx:encode(TextMsg)}, [], [])
    end,
    if Attachments == [] -> no_reply;
        true ->
            FileName = Get(<<"name">>, Attachments),
            FileMsg =
            case binary:split(FileName, [<<".">>]) of
                [_, Format] when (Format == <<"jpeg">>) or (Format == <<"jpg">>) ->
                    Message0 ++ [{<<"type">>, <<"picture">>}
                    , {<<"text">>, FileName}
                    , {<<"media">>, Get(<<"contentUrl">>, Attachments)}
                    , {<<"thumbnail">>, Get(<<"thumbnailUrl">>, Attachments)}];
                _ ->
                    Message0 ++ [{<<"type">>, <<"file">>}
                    , {<<"file_name">>, FileName}
                    , {<<"media">>, Get(<<"contentUrl">>, Attachments)}
                    , {<<"size">>, Get(<<"size">>, Attachments)}]
            end,
            httpc:request(post, {?SEND_VIBER_MSG, [{"X-Viber-Auth-Token", binary_to_list(AuthenToken)}], 
                    ?CONTENT_TYPE_JSON, jsx:encode(FileMsg)}, [], [])
    end.

make_text_reply_message(<<"facebook">>, Reply, Message) ->
	Message ++ [{<<"text">>, Reply#reply.text}];
make_text_reply_message(<<"skype">>, Reply, Message) ->
	Message ++ [{<<"text">>, Reply#reply.text}];
make_text_reply_message(_, _, Message) ->
    Message.

make_attachment_reply_message(<<"facebook">>, Reply, Message) ->
	Message ++ [{<<"attachments">>, Reply#reply.attachments}];
make_attachment_reply_message(<<"skype">>, Reply, Message) ->
	Message ++ [{<<"text">>, Reply#reply.text}];
make_attachment_reply_message(_, _, Message) ->
	Message.	

auth_header(AccesToken) ->
    FullToken = binary_to_list(AccesToken#bot_access_token.token_type) ++ " " 
                ++ binary_to_list(AccesToken#bot_access_token.access_token), 
    {"Authorization", FullToken}.

send_reply_message(_, _, _, 0) -> 
    no_request_to_send;
send_reply_message(Url, AccesToken, Body, NumberOfRetry) ->
    ContentType = "application/json",
    Headers = [auth_header(AccesToken), {"Content-Type", ContentType}],

    try httpc:request(post, {Url, Headers, ContentType, Body}, [{timeout, ?TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _Res}} = Result ->
            ?INFO_MSG("Send Request to Url: ~p; Res: ~p~n", [Url, Result]), 
            Result; 
        {ok, {{_, 201, _}, _, _Res}} = Result ->
            ?INFO_MSG("Send Request to Url: ~p; Res: ~p~n", [Url, Result]), 
            Result;      
        Err ->
            ?ERROR_MSG("#69 Reason: ~p~n", [Err]),
            send_reply_message(Url, AccesToken, Body, NumberOfRetry - 1)
    catch
        _Exception:Reason -> 
            ?ERROR_MSG("#69 Reason: ~p~n", [Reason]),
            send_reply_message(Url, AccesToken, Body, NumberOfRetry - 1)
    end.

get_attachment_from_skype(Attachment, AccesToken, WebApi) ->
	Token = binary_to_list(AccesToken#bot_access_token.token_type) ++ " "
			++ binary_to_list(AccesToken#bot_access_token.access_token),
	Body = [{<<"token">>, list_to_binary(Token)}
			, {<<"link">>, Attachment#attachment.url}
			, {<<"contentType">>, Attachment#attachment.type}],
    ApiFile = apifile(WebApi),
    try httpc:request(post, {ApiFile, [], ?CONTENT_TYPE_JSON, jsx:encode(Body)}, [{timeout, ?TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, Result}} ->
			jsx:decode(list_to_binary(Result));
        Err ->
            ?ERROR_MSG("Error: ~p~n", [Err]),
            []
    catch
        _Exception:Reason ->
            ?ERROR_MSG("Reason: ~p~n", [Reason]),
            []
    end.

get_attachment_from_viber(Attachment, AccesToken, WebApi) ->
    % Token = binary_to_list(AccesToken#bot_access_token.token_type) ++ " "
    %         ++ binary_to_list(AccesToken#bot_access_token.access_token),
    Body = [{<<"token">>, AccesToken}
            , {<<"link">>, Attachment#attachment.url}
            , {<<"contentType">>, Attachment#attachment.type}],
    ApiFile = apifile(WebApi),
    try httpc:request(post, {ApiFile, [], ?CONTENT_TYPE_JSON, jsx:encode(Body)}, [{timeout, ?TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, Result}} ->
            jsx:decode(list_to_binary(Result));
        Err ->
            ?ERROR_MSG("Error: ~p~n", [Err]),
            []
    catch
        _Exception:Reason ->
            ?ERROR_MSG("Reason: ~p~n", [Reason]),
            []
    end.

apifile(WebApi) ->
    BWebApi = list_to_binary(WebApi),
    case binary:split(BWebApi, <<"kites/">>) of
        [FileApi, _] -> binary_to_list(FileApi) ++ "skype-attachment/upload";
        [Api] -> binary_to_list(Api)
end.

sha1(Content) when is_atom(Content) ->
    sha1(atom_to_list(Content));
sha1(Content) when is_integer(Content) ->
    sha1(integer_to_list(Content));
sha1(Content) when is_binary(Content) ->
    sha1(binary_to_list(Content));
sha1(Content) when is_list(Content) ->
    <<X:160/big-unsigned-integer>> = crypto:hash(sha, Content),
    L = lists:flatten(io_lib:format("~40.16.0b", [X])),
    list_to_binary(L);
sha1(Content) ->
    Content.

% <presence
%     from='hag66@shakespeare.lit/pda'
%     id='djn4714'
%     to='coven@chat.shakespeare.lit/thirdwitch'>
%   <x xmlns='http://jabber.org/protocol/muc'>
%     <password>cauldronburn</password>
%   </x>
% </presence>
make_presence_join_room(RoomPin, RoomJID) ->
    BRoomJID = jlib:jid_to_binary(RoomJID),
    #xmlel{name = <<"presence">>,
            attrs = [{<<"to">>, BRoomJID}],
            children = [#xmlel{name = <<"x">>,
               attrs = [{<<"xmlns">>, ?NS_MUC}],
               children = [#xmlel{name = <<"password">>,
                                  children = [#xmlcdata{content = RoomPin}]}
                          ]}]}.

generate_token(User, Password) ->
    Ts = get_ms_timestamp(),
    Hash = binary_to_list(User) ++ binary_to_list(Password) ++ integer_to_list(Ts),
    sha1(Hash).

generate_resource(ID) when is_binary(ID) ->
    Ts = get_ms_timestamp(),
    Hash = binary_to_list(ID) ++ integer_to_list(Ts),
    sha1(Hash);

generate_resource(ID) when is_integer(ID) ->
    Ts = get_ms_timestamp(),
    Hash = integer_to_list(ID) ++ integer_to_list(Ts),
    sha1(Hash).

proc_name(Channel, ID) -> 
    binary_to_atom(<< Channel/binary, "_", ID/binary>>, latin1).

% gets a timestamp in ms from the epoch
get_ms_timestamp() ->
    {Mega, Secs, Micro} = os:timestamp(),
    Mega * 1000000 * 1000000 + Secs * 1000000 + Micro.

is_expired_time(<<>>, CreateTimestampAt) ->
   false;
is_expired_time(ExpiresIn, CreateTimestampAt) 
                        when is_binary(ExpiresIn) ->
    is_expired_time(binary_to_integer(ExpiresIn), 
                                CreateTimestampAt);
is_expired_time(ExpiresIn, CreateTimestampAt) ->
    ExpireTime = ExpiresIn* 1000000,
    Now = get_ms_timestamp(),
    if (Now - CreateTimestampAt) < ExpireTime ->
            false;
        true ->
            true
    end.

%% request http get to Url; if it fails, it will return Default value
request_http_get(Url, Default) ->
    try httpc:request(get, {Url, []}, [{timeout, ?TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, Result}} ->
            ?INFO_MSG("request_http_get: Result: ~p~n", [Result]),
            case catch jsx:decode(list_to_binary(Result)) of
                {'EXIT', Msg} -> 
                    ?ERROR_MSG("Cannot decode response: ~p, reason: ~p~n", [Result, Msg]),
                    Default;
                Content ->
                    Content
            end;            
        Err ->
            ?ERROR_MSG("Error: ~p~n", [Err]),
            Default
    catch
        _Exception:Reason -> 
            ?ERROR_MSG("Reason: ~p~n", [Reason]),
            Default
    end.
