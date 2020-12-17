-module(debug_ej).
-include("jlib.hrl").

-export([ts/0
        , simulate_web_db_insert/4]).

ts() ->
    {Mega, Sec, Micro} = os:timestamp(),
    trunc(Mega * 1000000000000 + Sec * 1000000 + Micro).

%% note :  
%% senderKey without OrgId 
%% fromKey without OrgId 
%% 
simulate_web_db_insert(From, RoomID, Body, MsgId) ->
    OrgId = get_org_id_from_web_db(RoomID),
    [RoomKey | _] = binary:split(RoomID, [<<"_">>]),
    [FUserKey | _] = binary:split(From#jid.luser, [<<"_">>]),
    Doc = [{org, OrgId}, {senderKey, FUserKey}, {body, Body}, {id, MsgId},
            {fromKey, RoomKey}, {receiverKey, <<>>}, {isModified, <<"false">>},
            {subtype, <<>>}, {type, <<"groupchat">>}, {<<"time">>, os:timestamp()}],
    mod_mongo:insert_msg(Doc).


get_org_id_from_web_db(RoomID) -> 
    case binary:split(RoomID, [<<"_">>]) of
    [_, ORGID] ->
      OrgRes = mod_mongo:fetch_company([{key, ORGID}]),
      case OrgRes of
        [] ->
          <<>> ;
        _ ->
          proplists:get_value(<<"_id">>, lists:last(OrgRes))
      end;
    _ ->
      <<>>
    end.