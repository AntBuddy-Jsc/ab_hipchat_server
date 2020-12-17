-module(set_webhook).

-export([set_webhook/2, insert_account_info/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(HEADER, "X-Viber-Auth-Token").
-define(CONTENT_TYPE_JSON, "application/json").
-define(SET_WEBHOOK_API, "https://chatapi.viber.com/pa/set_webhook").
-define(GET_ACCOUNT_INFO_API, "https://chatapi.viber.com/pa/get_account_info").

set_webhook(Account_KiteAppId_List, URL) -> 
	Event_Types = [<<"delivered">>, <<"seen">>, <<"failed">>, <<"subscribed">>
						, <<"unsubscribed">>, <<"conversation_started">>],
	lists:foldl(fun({Acc, KiteAppId}, Index) ->
					BIndex = integer_to_binary(Index),
					Bot_Id = << <<"viberpubacc_">>/binary, BIndex/binary >>,
					BURL = << URL/binary, "/viber/message/", Bot_Id/binary >>,  
					Body = [{<<"url">>, BURL}, {<<"event_types">>, Event_Types}],
					httpc:request(post, {?SET_WEBHOOK_API, [{?HEADER, Acc}], 
           						?CONTENT_TYPE_JSON, jsx:encode(Body)}, [], []),

					insert_account_info(Bot_Id, Acc, KiteAppId),
					Index + 1
				end, 1, Account_KiteAppId_List).

delete_webhook(AccountList) ->
	lists:foreach(fun(Acc) ->
					Body = [{<<"url">>, <<"">>}],
					Res = httpc:request(post, {?SET_WEBHOOK_API, [{?HEADER, Acc}], 
           						?CONTENT_TYPE_JSON, jsx:encode(Body)}, [], [])
				  end, AccountList).

insert_account_info(Bot_Id, Acc, KiteAppId) ->
	{_,{_, _, Res}} = httpc:request(post, {?GET_ACCOUNT_INFO_API, [{?HEADER, Acc}], 
           ?CONTENT_TYPE_JSON, jsx:encode([{}])}, [], []),

	BRes = list_to_binary(Res),
	Members = proplists:get_value(<<"members">>, jsx:decode(BRes)),
	AccInfo = lists:last(Members),
	AccID = proplists:get_value(<<"id">>, AccInfo),
	Doc = [{bot_app_id, Bot_Id}, {bot_account, [{id, AccID}, 
				{kite_app_id, list_to_binary(KiteAppId)}, {auth_token, list_to_binary(Acc)}]}],
	mod_mongo:create_social_bot(Doc).
