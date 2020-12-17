-module(mod_viber_bot_handler).

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
    ?INFO_MSG("handle_from_json: ~p~n", [Req]),
    {ok, ReqBody, _} = cowboy_req:body(Req),
    JsonBody = jsx:decode(ReqBody),
    Event = proplists:get_value(<<"event">>, JsonBody),
    case Event of
        <<"message">> ->
                Sender = proplists:get_value(<<"sender">>, JsonBody),
                Sender_ID = proplists:get_value(<<"id">>, Sender),
                Sender_Name = proplists:get_value(<<"name">>, Sender),
                Sender_Avatar = proplists:get_value(<<"avatar">>, Sender),
                MsgID = proplists:get_value(<<"message_token">>, JsonBody),
                Message = proplists:get_value(<<"message">>, JsonBody),
                MsgType = proplists:get_value(<<"type">>, Message),
                MsgText = 
                case proplists:get_value(<<"text">>, Message) of
                    undefined -> <<>>;
                    Text -> Text
                end,
                MsgMedia =
                case proplists:get_value(<<"media">>, Message) of
                    undefined -> <<>>;
                    MediaUrl -> MediaUrl
                end, 
                Bot_app_id = case element(13, Req) of
                                [BID] -> BID;
                                _ -> <<>>
                             end,
                ?INFO_MSG("Bot_app_id: ~p~n", [Bot_app_id]),
                Bot_Info = lists:last(mod_mongo:fetch_social_bot([{<<"bot_app_id">>, Bot_app_id}])),
                {_, [Bot_Account]} = proplists:get_value(<<"bot_account">>, Bot_Info),
                ?INFO_MSG("Bot_Account: ~p~n", [Bot_Account]),
                ProcName = mod_social_util:proc_name(proplists:get_value(<<"id">>, Bot_Account), Sender_ID),
                ?INFO_MSG("ProcName: ~p~n", [ProcName]),
                ViberMessage = {message, #message{%type=MsgType
                                            text=MsgText
                                            , id = integer_to_binary(MsgID)
                                            , attachments = [#attachment{url=MsgMedia, type
                                                                             = MsgType}]
                                            }},
                SenderInfo = [{<<"question">>, MsgText} | Sender],
                ?INFO_MSG("Sender: ~p~n", [SenderInfo]),

                case mod_social_session:get_process_social_c2s(ProcName) of 
                        Pid when is_pid(Pid) -> 
                            Pid ! ViberMessage;
                        _ -> 
                            ?INFO_MSG("Create Pid~n", []),
                            case ?SOCIAL_CS2:instance(#social_session{id = integer_to_binary(MsgID),  
                                                                    user_account = Sender,
                                                                    bot_account = Bot_Account,
                                                                    conversation = [{<<"question">>, MsgText}],
                                                                    channel_id = <<"viber">>,
                                                                    service_url = <<>>
                                                                    }, 
                                                        State#config.web_base) of
                                {ok, NewPid} ->
                                    mod_social_session:register_process_social_c2s(ProcName, NewPid), 
                                    NewPid !  ViberMessage;
                                error -> 
                                    error
                            end
                    end;
            _ ->
                ok
    end,
   
    Req3 = case cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], <<>>, Req) of
        {ok, Result} ->
            Result;
        Res ->
            Res
    end,
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
