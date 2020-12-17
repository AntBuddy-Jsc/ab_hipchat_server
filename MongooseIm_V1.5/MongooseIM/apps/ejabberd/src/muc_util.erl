-module(muc_util).

-include("ejabberd.hrl").
-include("mod_muc_room.hrl").
-include("showdme.hrl").
-include("jlib.hrl").


-export([get_members_of_room/1, 
		get_rooms_of_user/1, 
		get_rosters_of_user/1,
		get_rosters_of_user_with_affiliation/1,
		get_rosters_not_in_room/2,
		to_bin/1]).

-export([new_track_org/0,
		validate_orgid_from_web_db_xmpp_db/2,
		get_orgid_from_web_db/2,
		validate_mongo_response/1,
		get_muc_service/1]).

%% DEBUG

-export([get_room_affiliations/2]).

-define(TIMEOUT_WAITING_RESPONSE_FROM_ROOM, 2000).

-record(muc_room, {name_host, opts }).

new_track_org() ->
	catch ets:new(track_org, [named_table, public, {write_concurrency, false}, {read_concurrency, true}]).

%% Input : {RoomId, <<"conference.htklabs.com">>}
%% Output :  List of members of room
get_members_of_room(RoomKey)  -> 
	Room = mnesia:dirty_read(muc_room, RoomKey),
	case Room of
		[#muc_room{opts= Opts}] ->
			 proplists:get_value(affiliations, Opts, []);
        _ ->
			[] 
    end. 

%% Input :  {UserId, <<"htklabs.com">>}
%% Output :  List of rooms of member 
get_rooms_of_user(UserKey) ->
	Rooms = mnesia:dirty_read(user_room, UserKey),
	lists:map(fun(#user_room{room= Room}) ->
		Room
	end, Rooms).

%% Input :  {UserId, <<"htklabs.com">>}
%% Output :  List of rosters of member
get_rosters_of_user(UserKey) ->
	Rooms = get_rooms_of_user(UserKey),
	Res = 
	lists:flatmap(fun(Room) ->
			Patt = #user_room{room=Room, _ = '_'},
			lists:map(fun(#user_room{us= User}) ->
					User
			end, mnesia:dirty_match_object(Patt))
	end, Rooms),
	lists:usort(Res).

%% Input :  {UserId, <<"htklabs.com">>}
%% Output :  List of rosters of member with affiliation
get_rosters_of_user_with_affiliation(UserKey) ->
    Rooms = get_rooms_of_user(UserKey),
    Res = 
    lists:flatmap(fun(Room) ->
            Patt = #user_room{room=Room, _ = '_'},
            lists:map(fun(#user_room{us= {U,S}, affiliation = Affiliation}) ->
                    {{U, S, <<>>}, Affiliation}
            end, mnesia:dirty_match_object(Patt))
    end, Rooms), 
    lists:usort(Res).

%% Input: {{RoomId, <<"conference.htklabs.com">>}, {UserId, <<"htklabs.com">>}}
%% Output: Lists of rosters who don't belong to RoomId
get_rosters_not_in_room(UserKey, RoomKey) ->
	Rooms = get_rooms_of_user(UserKey),
    Res = lists:flatmap(fun(Room) ->
					if Room /= RoomKey ->
						Patt = #user_room{room=Room, _ = '_'},
						lists:map(fun(#user_room{us= {U,S}}) ->
								{U, S, <<>>} 
								end, mnesia:dirty_match_object(Patt));
					true -> [] end
			end, Rooms),
    lists:usort(Res).


to_bin(Val) when is_list(Val) ->
	list_to_binary(Val);
to_bin(Val) when is_integer(Val) ->
	integer_to_binary(Val);
to_bin(Val) ->
	Val. 



validate_orgid_from_web_db_xmpp_db(Server, ORGID) ->
	case ets:lookup(track_org, ORGID) of 
	[{_, _ObjectOrgId}] ->  true ;
	_ -> 
		OrgRes = mod_mongo:fetch_company([{key, ORGID}]),
		ValidateResponse = validate_mongo_response(OrgRes),
		case ValidateResponse of
			[] ->
				?ERROR_MSG("mod_muc: get object org from mongodb: no data. Retry with postgres ~p",[ORGID]),
				retry_validate_orgid_from_postgres(Server, ORGID);
			_ ->
				ObjectOrgId = proplists:get_value(<<"_id">>, lists:last(ValidateResponse)),
				ets:insert(track_org, {ORGID, ObjectOrgId}),
				true
		end
	end. 
	
get_orgid_from_web_db(Server, JID) ->
	case ejabberd_config:get_local_option({host_type, Server}) of
		default ->
			case binary:split(JID#jid.luser, [<<"_">>]) of
				[<<"bot">>,  _] -> undefined ;
				[_, ORGID] ->
					case ets:lookup(track_org, ORGID) of 
					[{_, ObjectOrgId}] when ObjectOrgId /= <<>> -> 
						ObjectOrgId ;
					_ -> 
						OrgRes = mod_mongo:fetch_company([{key, ORGID}]),
						ValidateResponse = validate_mongo_response(OrgRes),
						case ValidateResponse of
							[] -> undefined;
							_ ->
								ObjectOrgId = proplists:get_value(<<"_id">>, lists:last(ValidateResponse)),
								ets:insert(track_org, {ORGID, ObjectOrgId}),
								ObjectOrgId
						end
					end;
				_ ->
					undefined
			end;
		_ ->
			undefined
	end.

retry_validate_orgid_from_postgres(Server, ORGID) ->	
	case mod_company_odbc:check_org(Server, ORGID) of 
	true -> 
		ets:insert(track_org, {ORGID, <<>>}),
		true;
	_ ->
		?ERROR_MSG("muc_room: retry get org from postgres: no data",[ORGID]),  
		false
	end.


validate_mongo_response(MongoResp) ->
	Validate = lists:any(fun(RecProps) ->
		Err = proplists:get_value(<<"$err">>, RecProps, <<>>),
		if Err /= <<>> -> true ;
		true -> false 
		end 
	end, MongoResp),
	if Validate == true -> [];
	true -> MongoResp 
	end. 


get_room_affiliations(RoomId, Service) ->
    case mod_muc:room_jid_to_pid(RoomId, Service) of
        {ok, Pid} ->
            case catch gen_fsm:sync_send_all_state_event(Pid, get_affiliations, ?TIMEOUT_WAITING_RESPONSE_FROM_ROOM) of 
            Affiliations when is_list(Affiliations) -> 
            	lists:map(fun({{UserId, Host, _}, _}) -> {UserId, Host} end, Affiliations);
            Errs ->
            	?ERROR_MSG("muc_room:  fail to get affiliations from process, go to fetch from mnesia: ~p; ~p",[Pid, {RoomId, Service}, Errs]),
            	case catch mnesia:dirty_read(muc_room, {RoomId, Service}) of 
            	[{_, _, MucOpts}] ->
            		Affiliations =  proplists:get_value(affiliations, MucOpts),
            		lists:map(fun({{UserId, Host, _}, _}) -> {UserId, Host} end, Affiliations);
            	_ -> 
            		[]
            	end
            end; 
        Errs ->
        	?ERROR_MSG("muc_room:   fail to get pid of room: ~p; Errs: ~p",[{RoomId, Service}, Errs]),
            []
    end.

get_muc_service(ServerHost) ->
	gen_mod:get_module_opt_host(ServerHost, mod_muc, "conference.@HOST@").