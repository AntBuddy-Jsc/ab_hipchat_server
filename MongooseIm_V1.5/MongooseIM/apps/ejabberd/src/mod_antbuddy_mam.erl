-module(mod_antbuddy_mam).

-behaviour(gen_mod).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_antbuddy_stanza.hrl").

%% gen_mod 
-export([start/2, stop/1]).
%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-record(state, {host}). 

-define(PROCNAME, ejabberd_mod_antbuddy_mam).
-export([count_unread_message/5
		,process_unread_message/5
		, update_room_status/6
		, update_user_logout/2
		, process_sm_iq_status/3
		, delete_member_room/2
		, update_active_members/3]).
		% , count_unread_message_kite_room/2]).

-export([get_unread_message/8
		, report_overrun/1]).

%% for test
-export([test_insert_multi_record/4]).

start(Host, Opts) ->
	%% hook for counting message when user login sucessfull
    ejabberd_hooks:add(c2s_login_success, Host, ?MODULE, count_unread_message, 75),
    %% hook for update active/inactive status of room
    ejabberd_hooks:add(update_room_status, Host, ?MODULE, update_room_status, 75),
    %% hook to update database when user logout
    ejabberd_hooks:add(c2s_logout, Host, ?MODULE, update_user_logout, 75),
    ejabberd_hooks:add(delete_member_room, Host, ?MODULE, delete_member_room, 75),
    %% register iq_handler for chat 1-1
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM_UPDATE_STATUS,
                                  ?MODULE, process_sm_iq_status, parallel),
    mod_active_status_mnesia:start(),

    case catch ets:new(track_process_handle_unread, [public, named_table, {write_concurrency, true}, {read_concurrency, true}]) of
    	{'EXIT', Err} -> ?INFO_MSG("Error: ~p~n", [Err]), ok;
    	R -> ?INFO_MSG("Result: ~p~n", [R]), ok
    end,
    manager_pool:start_link(ejabberd_sup, ?MODULE, Host, [{pool_size, 100} | Opts]).

stop(Host) ->
	ejabberd_hooks:delete(c2s_login_success, Host, ?MODULE, count_unread_message, 75),
    ejabberd_hooks:delete(update_room_status, Host, ?MODULE, update_room_status, 75),
    ejabberd_hooks:delete(delete_member_room, Host, ?MODULE, delete_member_room, 75),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_MAM_UPDATE_STATUS).

init([Host, _Opts]) ->
    {ok, #state{host = Host}}.	

handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast({insert_record, JID, BObjectJID, Ts}, State) ->
	?DEBUG("#86: read-unread: insert_record: User: ~p, Server_User: ~p, ObjectID: ~p, 
        Status: ~p, Timestamp: ~p",[JID#jid.luser, JID#jid.lserver, BObjectJID, <<"0">>, Ts]),
	mod_active_status_odbc:add_status_user(State#state.host,
			{JID#jid.luser, JID#jid.lserver, BObjectJID, <<"0">>, Ts}),
    {noreply, State};


handle_cast({unread_message, SessionPid, Server, JID, FromList, ToList}, State) ->
	process_unread_message(SessionPid, Server, JID, FromList, ToList),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}. 

handle_info({StatusType, From, Server, ObjectJID, Status, Ts}, State) ->
	update_room_status_at_worker_process({StatusType, From, Server, ObjectJID, Status, Ts}),
	{noreply, State};

handle_info(_Msg, State) ->
	{noreply, State}. 

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, #state{host = _Host}) ->
	ok.

get_all_rosters_and_rooms(JID, FromList, ToList) ->
	RoomList = mod_company_odbc:get_room_list_of_user(
							JID#jid.luser, JID#jid.lserver),
	RemoveDuplicateDataRoom = sets:to_list(sets:from_list(RoomList)),
	RoomJIDs = lists:map(fun({RoomID, Service}) -> 
				{RoomID, Service}
		end, RemoveDuplicateDataRoom),
	LRosters = sets:to_list(sets:intersection(
                sets:from_list(FromList),
                sets:from_list(ToList))),
    LJID = jlib:jid_tolower(jlib:jid_remove_resource(JID)),
    Rosters = LRosters -- [LJID],
	RosterJIDs = lists:map(fun({U, S, _}) ->
					{U, S}
			end, Rosters),
	RoomJIDs ++ RosterJIDs.


count_unread_message(SessionPid, Server, JID, FromList, ToList) ->
	manager_pool:wcast(Server, ?MODULE, {unread_message, SessionPid, Server, JID, FromList, ToList}, next_worker).

process_unread_message(SessionPid, Server, JID, FromList, ToList) -> 
	% Proc = gen_mod:get_module_proc(Server, ?PROCNAME),
	WebObjectOrgId = muc_util:get_orgid_from_web_db(Server, JID),
	if WebObjectOrgId == undefined -> nothing_to_do ;
	true -> 
		ets:insert(track_process_handle_unread, {JID, self()}),
		ObjectJIDs = get_all_rosters_and_rooms(JID, FromList, ToList),
		case mod_active_status_odbc:get_timestamp_status_user(
	                            Server, JID#jid.luser, JID#jid.lserver) of
			[] ->
				%% add active/inactive status
				%insert_all_obj_to_odbc(Server, JID, ObjectJIDs);
				ok;
			FetchResult when is_list(FetchResult) ->
				%% Res = [{BObjectJID, {status, ts}}]
				%% Objects = [{Type, ObjectID, Service}]
				%% count unread message
				% OrgId = <<"f1c11cc4-d9dd-4839-bbb8-9e148cfaffdd">>,
				
				?DEBUG("#86: read-unread:  OrgId: ~p",[WebObjectOrgId]),
				Fun = 
					fun({BObjectJID, {<<"1">>, _Ts}}) ->
						handle_with_active_object(SessionPid, Server, JID, BObjectJID);
					({BObjectJID, {<<"0">> , Ts}}) ->
						ObjectJID = jlib:binary_to_jid(BObjectJID),
						{ServiceType, ObjServer} = get_server(ObjectJID#jid.lserver),
						case ejabberd_config:get_local_option({host_type, ObjServer}) of 
						default = HostType -> 	
							handle_with_inactive_object(Server, WebObjectOrgId, JID, ObjectJID, ServiceType, HostType, Ts);
						_ ->  
							[{ObjectJID#jid.luser, ObjectJID#jid.lserver}]
						end;	
					(_) ->
						[]
				end,
				_ODBCObjectJIDs = lists:flatmap(Fun, FetchResult),
				%NewObjectJIDs = lists:subtract(ObjectJIDs, ODBCObjectJIDs),
				%insert_all_obj_to_odbc(Server, JID, NewObjectJIDs);
				ok;
			_ ->
				nothing_to_do
		end
	end.

insert_all_obj_to_odbc(Server, JID, ObjectJIDs) ->
	if ObjectJIDs /= [] -> 
		Records = make_records(JID#jid.luser, JID#jid.lserver, lists:usort(ObjectJIDs)),
		mod_active_status_odbc:insert_multi_records(Server, Records);
	true -> nothing_to_do
	end.

% get_orgid_from_web_db(Server, JID) ->
% 	case ejabberd_config:get_local_option({host_type, Server}) of
% 		default ->
% 			case binary:split(JID#jid.luser, [<<"_">>]) of
% 				[_, ORGID] ->
% 					case ets:lookup(track_org, ORGID) of 
% 					[{_, ObjectOrgId}] ->
% 						ObjectOrgId ;
% 					_ -> 
% 						OrgRes = mod_mongo:fetch_company([{key, ORGID}]),
% 						case OrgRes of
% 							[] ->
% 								[];
% 							_ ->
% 								ObjectOrgId = proplists:get_value(<<"_id">>, lists:last(OrgRes)),
% 								ets:insert(track_org, {ORGID, ObjectOrgId})
% 						end;
% 				_ ->
% 					[]
% 			end;
% 		_ ->
% 			[]
% 	end.

%% BObjectJID without resources
handle_with_active_object(SessionPid, Server, JID, ObjectJID) when is_record(ObjectJID, jid) ->
	handle_with_active_object(SessionPid, Server, JID, jlib:jid_to_binary(ObjectJID));	 
handle_with_active_object(SessionPid, Server, JID, BObjectJID) -> 
	?DEBUG("#86: read-unread: SessionPid: ~p, Server: ~p, JID: ~p, BObjectJID: ~p",
									[SessionPid, Server, JID, BObjectJID]),
	ObjectJID = jlib:binary_to_jid(BObjectJID),
	case mod_active_status_mnesia:get_user_resource_object(
				JID#jid.luser, JID#jid.lserver, ObjectJID) of 
	[] -> 
		%% TODO : no user active on this object, remark object to inactive
		%% ingored all unread message between object timestamp and current timestamp 
		{CurrTimestamp, _} = jlib:get_timestamp(),
		mod_active_status_odbc:update_inactive_user(Server,
			{JID#jid.luser, JID#jid.lserver, BObjectJID, CurrTimestamp});
	Resources -> 
		lists:foreach(fun(R) -> 
			NewJID = jlib:make_jid(JID#jid.luser, JID#jid.lserver, R),
			SessionPid ! {update_active_members, ObjectJID, NewJID}
		end, Resources)
	end,
	[{ObjectJID#jid.luser, ObjectJID#jid.lserver}].

%% count unread message of all object except kite room
%% BObjectJID without resources
handle_with_inactive_object(_Server, OrgId, JID, BObjectJID, ServiceType, HostType, Timestamp) when is_binary(BObjectJID) ->
	handle_with_inactive_object(_Server, OrgId, JID, jlib:binary_to_jid(BObjectJID), ServiceType, HostType, Timestamp);	
handle_with_inactive_object(_Server, OrgId, JID, ObjectJID, ServiceType, HostType, Timestamp) ->
	?DEBUG("#86: read-unread: JID: ~p; ObjectJID: ~p; ServiceType: ~p; HostType: ~p; Timestamp: ~p",
						[JID, ObjectJID, ServiceType, HostType, Timestamp]),
	ObjUser = ObjectJID#jid.luser,
	case binary:split(ObjUser, [<<"_">>]) of
	%% Check ObjId/=ObjOrgKey. If not, ignore  
	[UUID, OrgKey] when UUID /= OrgKey ->  
		case binary:split(JID#jid.luser, [<<"_">>]) of
			%% Check ObjOrgKey==CurrentOrgKey. If not ignore
			[_, CurrentKey] when OrgKey == CurrentKey -> 
				send_number_of_unread_message(JID, ServiceType, UUID, ObjUser, Timestamp, OrgId, HostType, ObjectJID);
			_ -> nothing_to_do
		end;					
	[ObjUser] ->
		send_number_of_unread_message(JID, ServiceType, ObjUser, ObjUser, Timestamp, OrgId, HostType, ObjectJID);
	_ ->
		nothing_to_do
	end,
	[{ObjectJID#jid.luser, ObjectJID#jid.lserver}].


get_unread_message(_Type, JID, Server,  BObjectJID, <<"1">> = _Status, _Ts, _OrgId, _HostType) ->
	?DEBUG("#86: read-unread: Kite",[]),
	case ejabberd_sm:get_session_pid(JID#jid.luser, JID#jid.lserver, JID#jid.lresource) of
	none -> 
		unknown;
	SessionPid ->	 
		handle_with_active_object(SessionPid, Server, JID, BObjectJID)
	end;

get_unread_message(Type, JID, Server, BObjectJID, <<"0">> = _Status, Timestamp, OrgId, HostType) ->
	?DEBUG("#86: read-unread: Kite",[]),
	handle_with_inactive_object(Server, OrgId, JID, BObjectJID, Type, HostType, Timestamp);

get_unread_message(_Type,  _JID, _Server, _BObjectJID, _Status, _Ts, _OrgId, _HostType) ->
	unknown.


send_number_of_unread_message(JID, Type, UUID, LUser, Ts, OrgId, Host_Type, ObjectJID) ->
	Timestamp = <<Ts/binary, "Z">>,
	UTCTime = jlib:datetime_binary_to_timestamp(Timestamp),
	CountMessage = case Host_Type of
		default ->
			count_unread_message_from_web_db(
					JID, Type, UUID, UTCTime, OrgId);
		support ->
			count_unread_message_from_ejabberd_db(
					JID, Type, LUser, UTCTime);
		_ ->
			0
	end,
	if CountMessage > 0 ->
			Stanza = mod_antbuddy_stanza:count_unread_message(
											CountMessage, Timestamp),
			ejabberd_router:route(ObjectJID, JID, Stanza);
		true ->
			nothing_to_do
	end.

count_unread_message_from_web_db(JID, Type, UUID, UTCTime, OrgId) ->
	case Type of
        groupchat ->

            MongoResp = mod_mongo:fetch_msg([
                    {fromKey, UUID},
                    {time, [{gte, UTCTime}]}]),
			ValidateResp = muc_util:validate_mongo_response(MongoResp),
			length(ValidateResp);
        chat ->
            case binary:split(JID#jid.luser, [<<"_">>]) of
                [UJID, _] ->
                    NoMsg = mod_mongo:fetch_msg([
                        {fromKey, UUID},
                        {receiverKey, UJID},
                        {org, OrgId},
                        {time, [{gte, UTCTime}]}]),
                    ValidateResp = muc_util:validate_mongo_response(NoMsg),
                    length(ValidateResp);
                _ ->
                    0
            end;
        _ -> 0
    end.

count_unread_message_from_ejabberd_db(JID, Type, ObjectID, UTCTime) ->
	case Type of
        groupchat ->
            MongoResp = mod_mongo:fetch([
                    {fromKey, ObjectID},
                    {time, [{gte, UTCTime}]}]),
            ValidateResp = muc_util:validate_mongo_response(MongoResp),
			length(ValidateResp);

        chat ->
        	UJID = jlib:make_jid(JID#jid.luser, JID#jid.lserver, <<>>),	
            NoMsg = mod_mongo:fetch([
                {fromKey, ObjectID},
                {receiverKey, UJID},
                {time, [{gte, UTCTime}]}]),
            ValidateResp = muc_util:validate_mongo_response(NoMsg),
            length(ValidateResp);
        _ -> 0
    end.

%% update info data of status active/inactive with room/roster in
%% postgres (data is used to save inactive status and timestamp)
%% mnesia (data is used to save active status)
%% data of process (is is used to add ack to message stanza)
%% ObjectJID without resources
update_room_status_at_worker_process({update_active, From, Server, ObjectJID, Status, Ts}) ->
	ObjectJIDWithoutResource = jlib:jid_remove_resource(ObjectJID),
	CurrentObjectJID = mod_active_status_mnesia:get_user_resource_active(
													jlib:jid_tolower(From)),
	case jlib:are_equal_jids(ObjectJIDWithoutResource, CurrentObjectJID) of
		true ->
			nothing_to_do;
		false ->
			BObjectJID = jlib:jid_to_binary(ObjectJIDWithoutResource),
			?DEBUG("#86: read-unread:  active: UserId: ~p; BObjectJID: ~p; Status: ~p, Ts: ~p",
								[From#jid.luser, BObjectJID, Status ,Ts]),
			%% Insert or update record for active information
			mod_active_status_odbc:update_status_user(Server,
								{From#jid.luser, From#jid.lserver, BObjectJID, Status, Ts}),
			mod_active_status_mnesia:create_user_resource_active(
									jlib:jid_tolower(From), ObjectJIDWithoutResource),
			update_active_members(update_active_members, ObjectJIDWithoutResource, From),
			%% update record for inactive information of object
			update_inactive_user(From, Server, CurrentObjectJID, Ts)
	end;
update_room_status_at_worker_process({update_inactive, From, Server, ObjectJID, Status, Ts}) ->
	ObjectJIDWithoutResource = jlib:jid_remove_resource(ObjectJID),
	?DEBUG("#86: read-unread: inactive:  UserId: ~p; ObjectJID: ~p; Status: ~p, Ts: ~p",
								[From#jid.luser, ObjectJID, Status ,Ts]),
	mod_active_status_mnesia:delete_user_resource_active(
							jlib:jid_tolower(From), ObjectJIDWithoutResource),
	update_inactive_user(From, Server, ObjectJIDWithoutResource, Ts);

update_room_status_at_worker_process(Info) ->
	?DEBUG("#86: wrong info: ~p",[Info]),
	nothing_to_do.





%% route request active/inactive of one device to the same process 
%% to solve race condition 
update_room_status(_Type, _From, _Server, [], _Status, _Ts) ->
	nothing_to_do;
update_room_status(update_active, From, Server, ObjectJID, Status, Ts) ->
	case ets:lookup(track_process_handle_unread, From) of 
	[{_, Pid}] -> 
		%% handle at worker process where handle count message
		Pid ! {update_active, From, Server, ObjectJID, Status, Ts};
	_ ->
		update_room_status_at_worker_process({update_active, From, Server, ObjectJID, Status, Ts})
	end;	

update_room_status(add_status_active, From, Server, ObjectJID, _Status, Ts) ->
	BObjectJID = jlib:jid_to_binary(jlib:jid_remove_resource(ObjectJID)),
	manager_pool:wcast(Server, ?MODULE, {insert_record, From, BObjectJID, Ts}, next_worker);


update_room_status(update_inactive, From, Server, ObjectJID, Status, Ts) ->
	case ets:lookup(track_process_handle_unread, From) of 
	[{_, Pid}] -> 
		%% handle at worker process where handle count message
		Pid ! {update_inactive, From, Server, ObjectJID, Status, Ts};
	_ ->
		update_room_status_at_worker_process({update_inactive, From, Server, ObjectJID, Status, Ts})
	end;	


update_room_status(_, _, _, _, _, _) ->
    ok.

%% ObjectJID without resource
update_inactive_user(_, _, [], _) ->
	no_object_to_update;
update_inactive_user(From, Server, ObjectJID, Ts)->
	case mod_active_status_mnesia:get_user_resource_object(
							From#jid.luser, From#jid.lserver, ObjectJID) of
		[] ->
			BObjectJID = jlib:jid_to_binary(ObjectJID),
			mod_active_status_odbc:update_inactive_user(Server,
							{From#jid.luser, From#jid.lserver, BObjectJID, Ts}),
			update_active_members(update_inactive_members, ObjectJID, From);
		Resources ->
            %update_inactive_members message should be sent to c2s process of FromJID
            OnlineResouces = ejabberd_sm:get_user_resources(From#jid.luser, Server),
            lists:foreach(fun(R) ->
				case lists:member(R, OnlineResouces) of
					false ->
				mod_active_status_mnesia:delete_user_resource_active(
								{From#jid.luser, From#jid.lserver, R}, ObjectJID);
					_ -> ok
				end
            end, Resources),
			update_active_members(update_inactive_members, ObjectJID, From)
	end.

update_user_logout(Server, JID) ->
	{Ts, _} = jlib:get_timestamp(),
	ets:delete(track_process_handle_unread, JID),
	CurrentObjectJID = mod_active_status_mnesia:get_user_resource_active(
													jlib:jid_tolower(JID)),
	update_room_status(update_inactive, JID, Server, CurrentObjectJID, <<"0">>, Ts).

process_sm_iq_status(_From, _To, #iq{type = get}) ->
	{error, ?ERR_NOT_ALLOWED};
process_sm_iq_status(From, To, #iq{type = set, sub_el = SubEl} = IQ) ->
	Status = xml:get_subtag_cdata(SubEl, <<"status">>),
	{Ts, _} = jlib:get_timestamp(),
	case Status of
		<<"1">> ->
			%% route a notify message to webserver
			Stanza = mod_antbuddy_stanza:notify_message(To),
			Service = jlib:make_jid(<<>>, From#jid.lserver, <<>>),
			ejabberd_router:route(Service, jlib:jid_remove_resource(From), Stanza),
			update_room_status(update_active, From, To#jid.lserver, To, Status, Ts);
		_ ->
			update_room_status(update_inactive, From, To#jid.lserver, To, Status, Ts)
	end,
	IQ#iq{type = result, sub_el = []};
process_sm_iq_status(_From, _To, _IQ) ->
	{error, ?ERR_NOT_ALLOWED}.

delete_member_room(JID, RoomJID) ->
    Server = case binary:split(RoomJID#jid.lserver, <<".">>) of
                [<<"conference">>, Server_Host] -> Server_Host;
                _ ->  RoomJID#jid.lserver
             end,                
	RoomName = jlib:jid_to_binary(RoomJID),
	mod_active_status_odbc:delete_user(Server, 
				{JID#jid.luser, JID#jid.lserver, RoomName}).

%% update active/inactive status
%% active:		update_active_members
%% inactive:	update_inactive_members 
update_active_members(Option, To, From) ->
	case ejabberd_sm:get_user_present_pids(
								From#jid.luser,
								From#jid.lserver) of
		Pids when is_list(Pids) ->
			lists:foreach(fun({_, Pid}) ->
				case Option of
					update_active_members ->
						Pid ! {update_active_members, To, From};
					update_inactive_members ->
						Pid ! {update_inactive_members, To, From};
					_ ->
						nothing_to_do
				end
			end, Pids);
		_ ->
			nothing_to_do
	end.

%% make records for multiple insert to postgresql
make_records(User, Server, ObjectList) ->
	{Ts, _} = jlib:get_timestamp(),
	[HeadObject | TailObjects] = ObjectList,
	{HeadObjectID, HeadHost} = HeadObject,
	HeadJID = <<HeadObjectID/binary, "@", HeadHost/binary>>,
	HeadRecord = list_to_binary([<<"('">>, User, <<"', '">>, Server, <<"', '">>, 
					HeadJID, <<"', '0', '">>, Ts, <<"', '">>, Ts, <<"') ">>]),
	Res = lists:map(fun({ObjectID, Host}) -> 
			ObjectJID = <<ObjectID/binary, "@", Host/binary>>,
			R = [<<", ('">>, User, <<"', '">>, Server, <<"', '">>, 
				ObjectJID, <<"', '0', '">>, Ts, <<"', '">>, Ts, <<"') ">>],
			list_to_binary(R)
		end, TailObjects),
	Records = [HeadRecord | Res],
	list_to_binary(Records).
	
%% groupchat :  conference 
%% else:  chat 
get_server(Service) ->
	case binary:split(Service, <<".">>) of
		[<<"conference">>, ServerHost] -> 
			{groupchat, ServerHost};
		_ -> 
			{chat, Service}
	end.

report_overrun(Report) ->
	lager:error("~p", [Report]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% for testing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
                        [lists:nth(random:uniform(length(AllowedChars)),
                                   AllowedChars)]
                            ++ Acc
                end, [], lists:seq(1, Length)).	

make_object_jid(chat) ->
	Head = list_to_binary(get_random_string(16, "1234567890qwertyuiop")),
	Tail = list_to_binary(get_random_string(16, "asdfghjklzxcvbnm")),
	<< Head/binary, "_", Tail/binary>>;
make_object_jid(groupchat) ->
	Head = list_to_binary(get_random_string(16, "qwertyuiozxcvbnmth78")),
	Tail = list_to_binary(get_random_string(16, "asdfghjkl5678901")),
	<< Head/binary, "_", Tail/binary>>.

test_insert_multi_record(User, Server, Resource, Numbers) ->
	JID = jlib:make_jid(User, Server, Resource),
	Proc = gen_mod:get_module_proc(Server, ?PROCNAME),
	ObjectLists = lists:map(fun(_) ->
					case random:uniform(2) of
						1 ->		
							LUserObject = make_object_jid(chat), 
							{chat, LUserObject, Server};
						_ ->
							LUserObject =  make_object_jid(groupchat),
							MucHost = <<"conference.", Server/binary>>,
							{groupchat, LUserObject, MucHost}
					end
			end, lists:seq(1, Numbers)),
	?INFO_MSG("Test: ~p~n", [ObjectLists]),
	gen_server:cast(Proc, {insert_multi_records, JID, ObjectLists}).



