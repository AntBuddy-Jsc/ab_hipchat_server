%% author: Linh Tran
%%
-module(mod_antbuddy_stanza).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_antbuddy_stanza.hrl").

-export([
		count_unread_message/2
		, notify_message/0
		, notify_message/1
        , create_stanza_message/4
        , convert_value_to_message/3
        , item_to_binary/1
        , notify_kite_room_active/2]).

count_unread_message(CountMessage, Timestamp) ->
	CountMsgEle = #xmlel{
        name = <<"count">>,
        children = [#xmlcdata{content = integer_to_binary(CountMessage)}]
	},
	TsEle = [#xmlel{
        name = <<"timestamp">>,
        children = [#xmlcdata{content = Timestamp}]
	}],
	EventEl = #xmlel{
        name = <<"event">>,
        attrs = [{<<"xmlns">>, ?NS_MAM_COUNT_UNREAD_MESSAGE}],
        children = [CountMsgEle|TsEle]},
	 #xmlel{
        name = <<"message">>,
        attrs = [{<<"type">>, <<"headline">>}],
        children = [EventEl]}.

notify_message() ->
    EventEl = #xmlel{
        name = <<"event">>,
        attrs = [{<<"xmlns">>, ?NS_MAM_ACTIVE_ROOM}],
        children = []},
    #xmlel{
        name = <<"message">>,
        attrs = [{<<"type">>, <<"headline">>}],
        children = [EventEl]}.

notify_message(To) ->
	JID = jlib:jid_to_binary(jlib:jid_remove_resource(To)),
	InfoActive = #xmlel{
        name = <<"active">>,
        children = [#xmlcdata{content = JID}]},
	EventEl = #xmlel{
        name = <<"event">>,
        attrs = [{<<"xmlns">>, ?NS_MAM_ACTIVE_ROOM}],
        children = [InfoActive]},
	#xmlel{
        name = <<"message">>,
        attrs = [{<<"type">>, <<"headline">>}],
        children = [EventEl]}.

convert_value_to_message(JIDFrom, JIDTo, Record) ->
    case {proplists:get_value(<<"oembed">>, Record),
            proplists:get_value(<<"expandBody">>, Record),
            proplists:get_value(<<"file">>, Record),
            proplists:get_value(<<"beeiq">>, Record)} of
        {undefined, undefined, undefined, undefined} ->
            create_stanza_message(
                                Record, JIDFrom, JIDTo, normal_message);
        {OembedInfo, undefined, undefined, undefined} ->
            create_stanza_message(
                                Record, JIDFrom, JIDTo, {oembed, OembedInfo});
        {undefined, undefined, FileInfo, undefined} ->
            create_stanza_message(
                                Record, JIDFrom, JIDTo, {file, FileInfo});
        {undefined, ExpandBody, undefined, undefined} ->
            create_stanza_message(
                                Record, JIDFrom, JIDTo, {bot_message, ExpandBody});
        {undefined, undefined, undefined, BeeInfo} ->
            create_stanza_message(
                                Record, JIDFrom, JIDTo, {beeiq_message, BeeInfo});
        {undefined, undefined, File, Bee} ->
            create_stanza_message(
                                Record, JIDFrom, JIDTo, {beeiq_file_message, Bee, File});
        _ -> %% normal message
            create_stanza_message(
                                Record, JIDFrom, JIDTo, normal_message)
    end.

create_fileinfo_ele_message(FileInfo) ->
    #xmlel{name = <<"file">>,
            children = skip_undefined([
                create_subelement(?FILE_THUMBNAIL_URL, FileInfo),
                create_subelement(?FILE_THUMBNAIL_WIDTH, FileInfo),
                create_subelement(?FILE_THUMBNAIL_HEIGHT, FileInfo),
                create_subelement(?MIMETYPE, FileInfo),
                create_subelement(?FILE_URL, FileInfo),
                create_subelement(?FILE_SIZE, FileInfo),
                create_subelement(?FILE_NAME, FileInfo)
                ])}.

create_oembed_ele_message(OembedInfo) ->
    #xmlel{name = <<"oembed">>,
        children = skip_undefined([
            create_subelement(?PROVIDER_URL, OembedInfo),
            create_subelement(?PROVIDER_NAME, OembedInfo),
            create_subelement(?TITLE, OembedInfo),
            create_subelement(?AUTHOR_NAME, OembedInfo),
            create_subelement(?AUTHOR_URL, OembedInfo),
            create_subelement(?OEMBED_THUMBNAIL_URL, OembedInfo),
            create_subelement(?OEMBED_HTML, OembedInfo)
            ])}.

create_beeinfo_ele_message(BeeInfo) ->
    #xmlel{name = <<"kite">>,
            children = skip_undefined([
                create_subelement(<<"kite_app_name">>, BeeInfo),
                create_subelement(<<"avatar">>, BeeInfo),
                create_subelement(<<"provider">>, BeeInfo),
                create_subelement(<<"type">>, BeeInfo)
                ])}.

skip_undefined(Xs) ->
    [X || X <- Xs, X =/= undefined].

create_subelement(_Name, undefined) ->
    undefined;
create_subelement(Name, ItemInfo) ->
    Child = proplists:get_value(Name, ItemInfo, <<>>),
    #xmlel{
        name = Name,
        children = [#xmlcdata{content = Child}]
    }.

create_stanza_message(Record, From, To, TypeMessage) ->
    Body = item_to_binary(proplists:get_value(<<"body">>, Record, <<>>)),
    BodyEle = #xmlel{name = <<"body">>, children = [#xmlcdata{content = Body}]},
    Attrs = set_attributes_message(Record),
    Type = item_to_binary(proplists:get_value(<<"type">>, Record)),
    Subtype = item_to_binary(proplists:get_value(<<"subtype">>, Record)),
    NewAttrs = Attrs ++ [{<<"subtype">>, Subtype}],
    StandardEle = case Type of
        <<"groupchat">> ->
            Nick = item_to_binary(proplists:get_value(<<"senderKey">>, Record)),
            NickEle = #xmlel{name = <<"nick">>, children = [#xmlcdata{content = Nick}]},
            [NickEle, BodyEle];
        _ ->
            [BodyEle]
    end,
    case TypeMessage of 
        normal_message ->
            #xmlel{name = <<"message">>,
                attrs = NewAttrs,
                children = lists:reverse(StandardEle)};
        {oembed, OembedInfo} ->
            OembedEle = create_oembed_ele_message(OembedInfo),
            #xmlel{name = <<"message">>,
                    attrs =  NewAttrs,
                    children = lists:reverse([OembedEle | StandardEle])};
        {file, FileInfo} ->
            FileEle = create_fileinfo_ele_message(FileInfo),
            #xmlel{name = <<"message">>,
                    attrs = NewAttrs,
                    children = lists:reverse([FileEle | StandardEle])};
        {bot_message, ExpandBody} ->
            %Subtype = proplists:get_value(<<"subtype">>, Record),
            ExpandBodyEle = #xmlel{name = <<"expandbody">>, 
                            children = [#xmlcdata{content = ExpandBody}]},
            #xmlel{name = <<"message">>,
                    attrs = NewAttrs,
                    children = lists:reverse([ExpandBodyEle | StandardEle])};
        {beeiq_message, BeeInfo} ->
            BeeEle = create_beeinfo_ele_message(BeeInfo),
            #xmlel{name = <<"message">>,
                    attrs =  NewAttrs,
                    children = lists:reverse([BeeEle | StandardEle])};
        {beeiq_file_message, Bee, File} ->
            Bee_Ele = create_beeinfo_ele_message(Bee),
            File_Ele = create_fileinfo_ele_message(File),
            #xmlel{name = <<"message">>,
                    attrs =  NewAttrs,
                    children = lists:reverse([Bee_Ele, File_Ele | StandardEle])};
        _ ->
            []
    end.

set_attributes_message(Record) ->
    Get = fun proplists:get_value/2,
    Type = item_to_binary(Get(<<"type">>, Record)),
    Id = item_to_binary(Get(<<"id">>, Record)),
    Timestamp = item_to_binary(Get(<<"time">>, Record)),
    BTimestamp = jlib:now_to_utc_binary_milisecs(Timestamp),
    [{<<"id">>, Id},
        {<<"type">>, Type},
        {<<"timestamp">>, BTimestamp}].

item_to_binary(Item) when is_binary(Item)->
    Item;

item_to_binary(Item) when is_integer(Item)->
    integer_to_binary(Item);

item_to_binary(Item) when is_list(Item) ->
    list_to_binary(Item);

item_to_binary(Item) when is_float(Item) ->
    float_to_binary(Item);

item_to_binary(Item) when is_atom(Item) ->
    atom_to_binary(Item, utf8);

item_to_binary(Item) ->
    Item.

% <message
%     from='kite.antbuddy.com'
%     id='5BCE07C5-0729-4353-A6A3-ED9818C9B498'
%     to='user1_company1@htklabs.com/pda'
%     type='headline'>
%     <event xmlns='urn:xmpp:mam:1#kite_active_room'>
%          <item>kiteroom1_company1@kite.antbuddy.com</item>
%          <status>1</status>
%     </event>
% </message> 
notify_kite_room_active(KiteRoom, Type) ->
    JID = jlib:jid_to_binary(KiteRoom),
    RoomInfo = #xmlel{
        name = <<"item">>,
        children = [#xmlcdata{content = JID}]},
    ActiveInfo = [#xmlel{
        name = <<"type">>,
        children = [#xmlcdata{content = atom_to_binary(Type, utf8)}]}],
    EventEl = #xmlel{
        name = <<"event">>,
        attrs = [{<<"xmlns">>, ?NS_MAM_KITE_ACTIVE_ROOM}],
        children = [RoomInfo | ActiveInfo]},
    #xmlel{
        name = <<"iq">>,
        attrs = [{<<"type">>, <<"set">>}],
        children = [EventEl]}.