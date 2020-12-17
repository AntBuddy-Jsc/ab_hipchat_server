-module(mod_social_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-include("mod_social.hrl").

-record(config, {  
                    web_base     :: string()
                }).
-define(SOCIAL_CS2, mod_social_c2s).

init(_Type, Req, [_Host, Opts]) ->
    %BotApi = proplists:get_value(bot_api, Opts),
    WebUrl = proplists:get_value(web_api, Opts),
	{ok, Req, #config{  web_base = WebUrl }}.

handle(Req, State) ->
    {ok, Auth, Req1} = cowboy_req:parse_header(<<"authorization">>, Req),
    NewReq =
    case Auth of 
        {<<"bearer">>, Token} ->
            {ok, Body, Req2} = cowboy_req:body(Req1),
            Json = jsx:decode(Body),
            ?DEBUG("Pid: ~p, Request: ~p~n", [self(), Json]),
            ?INFO_MSG("Request Body: ~p~n", [Json]),
            Type = proplists:get_value(<<"type">>, Json),
            case Type of 
                <<"message">> -> 
                    ChannelId = proplists:get_value(<<"channelId">>, Json),
                    BotAccount = proplists:get_value(<<"recipient">>, Json),
                    Chk =
                    if ChannelId == <<"skype">> -> true;
                        true -> 
                             check_if_facebook_chat_enabling_with_bot(BotAccount, ChannelId)
                    end,
                    case Chk of
                        true ->
                            UserAccount = proplists:get_value(<<"from">>, Json),
                            MsgId = proplists:get_value(<<"id">>, Json),
                            Text = proplists:get_value(<<"text">>, Json, <<"">>),
                            ?INFO_MSG("Text: ~ts~n", [Text]),
                            JsonAttachs = proplists:get_value(<<"attachments">>, Json, []),
                            Attachments = lists:map(fun (Attach) -> 
                                                        #attachment{type = proplists:get_value(<<"contentType">>, Attach),
                                                                    url  = proplists:get_value(<<"contentUrl">>, Attach)}
                                                    end, JsonAttachs),

                            ServiceUrl = proplists:get_value(<<"serviceUrl">>, Json),
                            Conversation = proplists:get_value(<<"conversation">>, Json),
                            ConversationId = proplists:get_value(<<"id">>, Conversation),
                             
                            ProcName = mod_social_util:proc_name(ChannelId, ConversationId), % Procname is conversation id

                            %% add c2s process to supervisor
                            Message = {message, #message{id=MsgId
                                                    , text=Text
                                                    , attachments=Attachments
                                                    , authorization = Token}},
        					case mod_social_session:get_process_social_c2s(ProcName) of 
                                Pid when is_pid(Pid) -> 
                                    Pid ! Message;
                                _ -> 
                                    case ?SOCIAL_CS2:instance(#social_session{id = ConversationId,  
                                                                            user_account = UserAccount ,
                                                                            bot_account = BotAccount,
                                                                            conversation = [{<<"question">>, Text}, {<<"msg_id">>, MsgId} | Conversation],
                                                                            channel_id = ChannelId,
                                                                            service_url = ServiceUrl}, 
                                                                State#config.web_base) of
                                        {ok, NewPid} ->
        									mod_social_session:register_process_social_c2s(ProcName, NewPid), 
                                            NewPid !  Message;
                                        error -> 
                                            error
                                    end
                            end;
                        _ ->
                            ?ERROR_MSG("#69 Disable Facebook chat ~n", [])
                    end;
                _ -> 
                    do_no_thing
            end, 
            Req2;
        _ -> 
            ?ERROR_MSG("#69 Request without authorization token ~p", []),
            Req1
    end,

    Req3 = case cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<>>, NewReq) of
        {ok, Result} ->
            Result;
        Res ->
            Res
    end,
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

check_if_facebook_chat_enabling_with_bot(BotAccount, ChannelId) ->
    if ChannelId == <<"facebook">> ->
        BotId = proplists:get_value(<<"name">>, BotAccount),
        case ets:lookup(track_social_bot, BotId) of
            [{_, BotCacheInfo}] -> true;
            _ ->
                case mod_mongo:fetch_social_bot([{<<"bot_app_id">>, BotId}]) of
                    [] ->
                        false;
                    BotInfo ->
                        Info = lists:last(BotInfo),
                        MSAppID = proplists:get_value(<<"ms_app_id">>, Info),  
                        case mod_mongo:fetch_social_bot([{<<"ms_app_id">>, MSAppID}, 
                                {<<"kite_app_id">>, <<>>}]) of
                            [] -> ets:insert(track_social_bot, {BotId, Info}), 
                                  true;
                            _ -> false
                        end
                end
        end;
          true -> true
    end.
