-module(mod_antbuddy_message_worker).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_antbuddy_stanza.hrl").

%% gen_server 
-export([init/1, handle_call/3, terminate/2, handle_info/2, code_change/3, handle_cast/2]).
-export([push_normal/2]).

% test
-export([test_push_notification/5, test_push_performance/1, test_push_performance/6]).

%% Macros
-define(PROCNAME, ejabberd_antbuddy_message).
-define(CONTENT_TYPE, "application/json").
-define(MAX_LENGTH_CONTENT, 128).
-define(TIMEOUT, 10000).

push_normal(Lserver, {push_normal, [_From, _To, _Stanza]} = Msg) ->
    manager_pool:wcast(Lserver, ?MODULE, Msg, next_worker);

push_normal(_Lserver, _Msg) ->
    ok.


init(_Args) -> {ok, []}.

handle_call(_Request, _From, State) -> 
        {reply, ok, State}.

terminate(_Reason, _State) -> ok.

handle_info(_Info, State) -> {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_cast({push_normal, [From, To, Stanza]}, State) -> 
    Type = xml:get_tag_attr_s(<<"type">>, Stanza),
    MsgId = xml:get_tag_attr_s(<<"id">>, Stanza),
    case mod_antbuddy_message:check_always_push_message(Stanza) of
        true ->
            Content = case xml:get_subtag_cdata(Stanza, ?EVENT) of
				<<>> ->
					xml:get_subtag_cdata(Stanza, <<"body">>);
				Event ->
					Event
			end,
            Timestamp = xml:get_tag_attr_s(<<"timestamp">>, Stanza),
            case Type of
                <<"chat">> ->
                    handle_chat_push(Type, <<"false">>, From, To, Content, Timestamp, MsgId);
                    % set_content_message_for_chat_api(<<"false">>, From, To, Content, Timestamp);
                <<"groupchat">> ->
					SubType = xml:get_tag_attr_s(<<"subtype">>, Stanza),
                    Members = get_all_members_room(From, To),
                    handle_groupchat_push(Type, <<"false">>, Members, From, To, Content, Timestamp, SubType, MsgId);
                    % set_content_message_for_groupchat_api(<<"false">>, Members, From, To, Content, Timestamp, SubType);
                _ ->
                    []
            end; 
        false ->
            check_push_notification(Type, From, To, Stanza, MsgId)
    end,
    {noreply, State};
handle_cast({push_invite, [From, To, RoomJID]}, State) ->
    [FromKey|_] = binary:split(From#jid.luser, <<"_">>),
    [ToKey|_] = binary:split(To#jid.luser, <<"_">>),   
    Body = convert_body_to_json(<<"invite-room">>, <<"true">>, [ToKey], 
							FromKey, RoomJID#jid.luser, <<>>, <<>>, <<>>, <<>>),
    case check_receiver_online(To) of
        true ->
            nothing_to_push;
        _ ->
            send_push_notification(<<>>, To, Body)
    end,
    {noreply, State}.

send_push_notification(Type, To, Body) ->
    Server = case Type of
        <<"groupchat">> ->
            [_| Sv] = binary:split(To#jid.lserver, <<".">>),
            lists:last(Sv);
        _ ->
            To#jid.lserver
    end,
   mod_rabbitmq_message:publish_message(Server, Body).

%% TODO :  send push notification based on presence status 

% check_push_notification(Type, From, To, Stanza) ->
%     Timestamp = xml:get_tag_attr_s(<<"timestamp">>, Stanza),
%     Body = case Type of
%         <<"chat">> ->
%             case check_receiver_online(To) of
%                 true ->
%                     [];
%                 _ ->
%                     Content = get_content_message(Stanza),
%                     handle_chat_push(Type, <<"true">>, From, To, Content, Timestamp);
%                    %%set_content_message_for_chat_api(<<"true">>, From, To, Content, Timestamp)
%             end;
%         <<"groupchat">> ->
%             Members = get_user_offline_of_room(From, To),
%             if length(Members) > 0 ->
%                    Content = get_content_message(Stanza),
% 					 SubType = xml:get_tag_attr_s(<<"subtype">>, Stanza),
%                    handle_groupchat_push(Type, <<"true">>, Members, From, To, Content, Timestamp, SubType);
%                    %%set_content_message_for_groupchat_api(<<"true">>, Members, From, To, Content, Timestamp, SubType);
%                 true ->
%                     []
%             end;
%         _ ->
%             []
%     end.

%%
%% TODO :  send push notification on all messages without consider presence status
%% Online :  send push notification with flag send_email = false
%% Offline : send push notification with flag send_email = true
check_push_notification(Type, From, To, Stanza, MsgId) ->
    Timestamp = xml:get_tag_attr_s(<<"timestamp">>, Stanza),
    case Type of
        <<"chat">> ->
            IsSendEmail = case check_receiver_online(To) of
            true -> <<"false">>;
            _ -> <<"true">>
            end,
            Content = get_content_message(Stanza),
            handle_chat_push(Type, IsSendEmail, From, To, Content, Timestamp, MsgId); 
        <<"groupchat">> ->
            AllMembers = get_all_members_room(From, To),
            OfflineMembers = get_user_offline_of_room(AllMembers),
            OnlineMembers = lists:subtract(AllMembers, OfflineMembers),
            Content = get_content_message(Stanza),
            SubType = xml:get_tag_attr_s(<<"subtype">>, Stanza),
            %lager:debug("#AB_84: AllMembers: ~p; OfflineMembers: ~p;  OnlineMembers: ~p",
            %                            [AllMembers, OfflineMembers, OnlineMembers]),
            handle_groupchat_push(Type, <<"true">>, OfflineMembers, From, To, Content, Timestamp, SubType, MsgId),
            handle_groupchat_push(Type, <<"false">>, OnlineMembers, From, To, Content, Timestamp, SubType, MsgId);
        _ ->
            []
    end.

check_receiver_online(JID) ->
    Pids = ejabberd_sm:get_user_present_pids(JID#jid.luser, JID#jid.lserver),
    case Pids of
        [] ->
            false;
        _ ->
            get_show_of_presence(Pids) 
    end.

get_show_of_presence([]) ->
    false;
get_show_of_presence([{_, Pid} | _]) when Pid == self()->
    true;
get_show_of_presence([{_, Pid} | Pids]) ->
    Show = try ejabberd_c2s:get_presence(Pid) of
		{_User, _Resource, ShowPresence, _Status} ->
			ShowPresence
	catch
		_Exception:Reason ->
			?ERROR_MSG("Reason: ~p~n", [Reason]),
			<<>>
	end,
    case Show of
        <<"available">> ->
            true;
        <<"online">> ->
            true;
        _ ->
            get_show_of_presence(Pids)
    end.

get_content_message(Stanza) ->
    case xml:get_subtag(Stanza, <<"file">>) of
        false ->
            xml:get_subtag_cdata(Stanza, <<"body">>);
        FileData ->
            get_content_message_of_file(Stanza, FileData)
    end.

get_content_message_of_event(Stanza, Event) ->
    case {xml:get_tag_attr_s(<<"type">>, Event),
          xml:get_subtag_cdata(Event, <<"point">>)} of
        {<<"add-user-bonus">>, false} ->
            [];
        {<<"add-user-bonus">>, Point} ->
            case xml:get_subtag_cdata(Event, <<"message">>) of
                <<>> ->
                    <<"has sent ", Point/binary, " points">>;
                Msg ->
                    <<"has sent ", Point/binary, " points: ", Msg/binary>>
            end;
        _ ->
            xml:get_subtag_cdata(Stanza, <<"body">>)
    end.

get_content_message_of_file(Stanza, FileData) ->
    case xml:get_subtag(FileData, <<"oembed">>) of
        false ->
            case xml:get_subtag_cdata(FileData, ?MIMETYPE) of
                false ->
                    xml:get_subtag_cdata(Stanza, <<"body">>);
                MimeType ->
                    FileName = xml:get_subtag_cdata(FileData, ?FILE_NAME),
                    case binary:split(MimeType, <<"/">>) of
                        [<<"image">>, _] ->
                            <<"Sent an image ", FileName/binary>>;
                        [_, _] ->
                            <<"Sent a file ", FileName/binary>>;
                        _ ->
                            xml:get_subtag_cdata(Stanza, <<"body">>)
                    end
            end;
        _ ->
            []
    end.

get_all_members_room(From, Room) ->
    Members = case mnesia:dirty_read(muc_room, {Room#jid.luser, Room#jid.lserver}) of 
        [{_, _, Opts}] ->  
            proplists:get_value(affiliations, Opts);
        _ ->
            []
    end,
    lists:filter(fun({{U, S, _}, _}) ->
            case (From#jid.luser == U) andalso
                 (From#jid.lserver == S) of
                    true ->
                        false;
                    false ->
                        true
            end
        end, Members).


get_user_offline_of_room(From, Room) ->
    Affiliations = get_all_members_room(From, Room),
    lists:filter(fun({{U, S, R}, _}) ->
            JID = jlib:make_jid(U, S, R),
            not check_receiver_online(JID)
    end, Affiliations).

get_user_offline_of_room(Affiliations) ->
     lists:filter(fun({{U, S, R}, _}) ->
            JID = jlib:make_jid(U, S, R),
            not check_receiver_online(JID)
    end, Affiliations).


set_content_message_for_groupchat_api(_, _, _, _, <<>>, _, _, _) ->
    [];
set_content_message_for_groupchat_api(_, _, _, _, [], _, _, _) ->
    [];
set_content_message_for_groupchat_api(IsSendEmail, Members, From, Room, Content, Timestamp, SubType, MsgId) ->
    UserKeys = lists:map(fun({{U, _, _}, _}) ->
                [UserKey| _] = binary:split(U, <<"_">>),
                UserKey
        end, Members),
    [FromKey|_] = binary:split(From#jid.luser, <<"_">>),
    convert_body_to_json(<<"groupchat">>, IsSendEmail, 
				UserKeys, FromKey, Room#jid.luser, Content, Timestamp, SubType, MsgId).

set_content_message_for_chat_api(_, _, _, <<>>, _, _) ->
    [];
set_content_message_for_chat_api(_, _, _, [], _, _) ->
    [];
set_content_message_for_chat_api(IsSendEmail, From, To, Content, Timestamp, MsgId) ->
    [FromKey|_] = binary:split(From#jid.luser, <<"_">>),
    [ToKey|_] = binary:split(To#jid.luser, <<"_">>),
    convert_body_to_json(<<"chat">>, IsSendEmail, [ToKey], FromKey, From#jid.luser, Content, Timestamp, <<>>, MsgId).

convert_body_to_json(<<"groupchat">>, _, _, _, _, <<"undefined">>, _, _, _) ->
    [];
convert_body_to_json(<<"groupchat">>, _, [<<"bot">>], _, _, _, _, _, _) -> 
    [];
convert_body_to_json(Type, IsSendEmail, ToUsers, FromUser, DesRoom, Content, Timestamp, SubType, MsgId) ->
    Body = [{<<"type">>, Type},
			{<<"subtype">>, SubType},
            {<<"id">>, MsgId},
            {<<"email">>, IsSendEmail},
            {<<"toUsers">>, ToUsers}, 
            {<<"fromUser">>, FromUser}, 
            {<<"desRoom">>, DesRoom},
            {<<"body">>, Content},
            {<<"timestamp">>, Timestamp}],
    NewBody = case Type of
                <<"groupchat">> ->
                    Body ++ [{<<"roomType">>, <<"kite">>}];
                _ -> Body
              end,
    jsx:encode(NewBody).

handle_chat_push(Type, IsSendEmail, From, To, Content, Timestamp, MsgId) ->
    Body = set_content_message_for_chat_api(IsSendEmail, From, To, Content, Timestamp, MsgId),
    case Body of 
    [] -> ok ;
    _ ->
        % lager:debug("#AB_84: handle_chat_push: IsSendEmail: ~p; Type: ~p; To: ~p; Body: ~p",
        %                                                 [IsSendEmail, Type, To, Body]),   
        send_push_notification(Type, To, Body)
    end.

handle_groupchat_push(_Type, _IsSendEmail, [], _From, _To, _Content, _Timestamp, _SubType, _MsgId) ->
    ok;
handle_groupchat_push(Type, IsSendEmail, Members, From, To, Content, Timestamp, SubType, MsgId) ->
    Body = set_content_message_for_groupchat_api(IsSendEmail, Members, From, To, Content, Timestamp, SubType, MsgId),
    case Body of 
    [] -> ok ;
    _ -> 
        % lager:debug("#AB_84: handle_groupchat_push: IsSendEmail: ~p, Type: ~p; To: ~p; Body: ~p",
        %                                     [IsSendEmail, Type, To, Body]),
        send_push_notification(Type, To, Body)
    end.
%%%%%%%%%%%%%%%%%%%%%%
%% test
%%%%%%%%%%%%%%%%%%%%%%
test_push_notification(ToUsers, FromUser, DesRoom, Content, Server) ->
    Type = <<"chat">>, %%<<"groupchat">>
    Timestamp = <<"">>,
    Email = <<"false">>,
    Body = convert_body_to_json(Type, Email, ToUsers, FromUser, DesRoom, Content, Timestamp, <<>>, <<>>),
    PushUrls = ejabberd_config:get_local_option({push_notification_url, Server}),
    lists:foreach(fun(PushUrl) ->
        Url = PushUrl ++ "api/sendPushNotification",
        ?INFO_MSG("AB#51: Url: ~p, Body: ~p", [Url, Body]),
        httpc:request(post, {Url, [], ?CONTENT_TYPE, Body}, [], [{sync, false}])
    end, PushUrls).

test_push_performance(Num) -> 
    % Test account on dev server
    % ToUsers = [<<"fe377e40-f0d8-11e5-8359-a5a09b23cf44">>],
    % FromUser = <<"3a653190-f6fc-11e5-8bee-bbcc1eca4588">>,
    % DesRoom = <<"3a653190-f6fc-11e5-8bee-bbcc1eca4588_04ab3734-ce25-45b9-8209-56e775ffb7f8">>,
    % Content = <<"Load test">>,
    % Server = <<"htklabs.com">>,

    % Test account on stagging server for thuan account
    % ToUsers = [<<"c4a93e00-0b6f-11e6-802c-494fa6f9d7d4">>],
    % FromUser = <<"76afb1c0-0b6f-11e6-802c-494fa6f9d7d4">>,
    % DesRoom = <<"76afb1c0-0b6f-11e6-802c-494fa6f9d7d4_f1c11cc4-d9dd-4839-bbb8-9e148cfaffdd">>,
    % Content = <<"Load test stagging">>,
    % Server = <<"antbuddy.com">>,

    % Test account on stagging server for thuan account
    ToUsers = [<<"6b0da6b0-15a6-11e6-8d18-91c528c9c04c">>],
    FromUser = <<"55c11350-15a6-11e6-8d18-91c528c9c04c">>,
    DesRoom = <<"55c11350-15a6-11e6-8d18-91c528c9c04c_f57faee4-ec2c-4711-967c-37addcd3715e">>,
    Content = <<"Load test plx user 2">>,
    Server = <<"antbuddy.com">>,

    test_push_performance(Num, ToUsers, FromUser, DesRoom, Content, Server).
    
test_push_performance(0, _, _, _, _, _) -> ok; 
test_push_performance(Num, ToUsers, FromUser, DesRoom, Content, Server) -> 
    Type = <<"chat">>,
    Ts = os:timestamp(),
    Timestamp = jlib:now_to_utc_binary_milisecs(Ts),
    Email = <<"false">>,
    Body = convert_body_to_json(Type, Email, ToUsers, FromUser, DesRoom, Content, Timestamp, <<>>, <<>>),
    mod_rabbitmq_message:publish_message(Server, Body), 
    test_push_performance(Num - 1 , ToUsers, FromUser, DesRoom, Content, Server).

