%% This module for updating data related to muc room in PostgreSQL
-module(mod_company_odbc).

-export([start/2, stop/1]).

%% API for company_room
-export([add_company_room/4
		, delete_company_room/3
		, update_company_room/7
		, search_all_rooms_of_company/2
		, search_rooms_in_company/2
		, search_company_of_room/2
		, check_default_room/2
		, check_org/2]).

%% API for user_room
-export([add_new_user/3
		, update_user/3
		, delete_member/3
		, delete_member/2
		, get_user_rooms/2
		, get_room_members/2
		, remove_affiliations/2
		, get_rooms_of_user/1
		, get_default_room_of_user/1
		, get_room_list_of_user/2]).

-include("ejabberd.hrl").
-include("showdme.hrl").
-define(COLUMNS_USER_ROOM, 
		[<<"user_id">>, <<"server">>, <<"room_id">>, <<"service">>, <<"affiliation">>]).
start(_Host, _Opts) -> ok.

stop(_Host) -> ok.


%%----------------------------------------------
%% TODO: update a table company_room
%%----------------------------------------------

add_company_room(Server, CompanyId, RoomId, Flag) ->
	ejabberd_odbc:sql_transaction(
		Server,
		fun() ->
          ejabberd_odbc:sql_query_t(
			[<<"insert into company_room(company_id, room_id, default_flag) values ('">>, 
			CompanyId, <<"', '">>, RoomId, <<"', ">>, integer_to_binary(Flag), <<");">>])
		end).

delete_company_room(Server, CompanyId, RoomId) ->
	?INFO_MSG("AB#22 CompanyId ~p~n RoomId ~p~n", [CompanyId, RoomId]),
	ejabberd_odbc:sql_transaction(
		Server,
		fun() ->
          ejabberd_odbc:sql_query_t(
			[<<"delete from company_room where company_id = '">>, 
			CompanyId, <<"' and room_id = '">>, RoomId, <<"';">>])
		end).

update_company_room(Server, NewCompanyId, NewRoomId, NewFlag, OldCompanyId, OldRoomId, OldFlag) ->
	ejabberd_odbc:sql_transaction(
		Server,
		fun() ->
          ejabberd_odbc:sql_query_t(
			[<<"update company_room set company_id = '">>, NewCompanyId, <<"', room_id = '">>, NewRoomId, 
			<<"', default_flag = ">>, integer_to_binary(NewFlag), <<" where company_id = '">>, OldCompanyId, 
			<<"' and room_id = '">>, OldRoomId, <<"' and default_flag = ">>, integer_to_binary(OldFlag), <<";">>])
		end).

search_all_rooms_of_company(Server, CompanyId) ->
	Result = ejabberd_odbc:sql_query(
		Server,
		[<<"select room_id from company_room where company_id = '">>, CompanyId, <<"';">>]),
	case Result of
		{selected, [<<"room_id">>], []} ->
				[];
		{selected, [<<"room_id">>], Res} -> 
				Res;
		_ -> error
	end.

%%search all rooms that beongs to company. The result don't include company room
search_rooms_in_company(Server, CompanyId) ->
    Result = ejabberd_odbc:sql_query(
        Server,
        [<<"select room_id from company_room where company_id = '">>, 
								CompanyId, <<"' and default_flag = 0;">>]),
    case Result of
        {selected, [<<"room_id">>], []} ->
                [];
        {selected, [<<"room_id">>], Res} ->
				Res;
        _ -> error
    end.

search_company_of_room(Server, RoomId) ->
	Result = ejabberd_odbc:sql_query(
        Server,
        [<<"select distinct company_id from company_room where room_id = '">>, 
															RoomId, <<"';">>]),
	case Result of
        {selected, [<<"company_id">>], []} ->
                [];
        {selected, [<<"company_id">>], Res} ->
                Res;
        _ -> error
    end.

check_default_room(Server, Room) ->
	{Default, RoomId, CompanyId} = case binary:match(Room, [<<"_">>]) of
		nomatch ->
			{true, Room, Room};
		_ ->
			[RId | CId ]  = binary:split(Room, [<<"_">>]),
			{false, RId, CId}
	end,
	case Default of
		true ->
			Result = ejabberd_odbc:sql_query(Server, [<<"select count(*) from company_room where room_id = '">>, 
									RoomId, <<"' and company_id = '">>, CompanyId, <<"' and default_flag = 1;">>]),
			case Result  of 
				{selected, _, [{Num}]} when Num == <<"0">>  -> false ;
				{selected, _, [{_Num}]}  -> true;
				_ -> false 
			end;
		false ->
			false
	end.

check_org(Server, OrgId) ->
	Result = ejabberd_odbc:sql_query(Server, [<<"select count(*) from company_room where room_id = '">>, 
										OrgId, <<"' and company_id = '">>, OrgId, <<"' and default_flag = 1;">>]),
	case Result  of 
		{selected, _, [{Num}]} when Num == <<"0">>  -> false ;
		{selected, _, [{_Num}]}  -> true;
		_ -> false 
	end. 


%%----------------------------------------------
%% TODO: update a table user_room
%%----------------------------------------------
add_new_user({User, Server}, {Name, Host}, Affilation) ->
	Host_Server = mod_muc_company:get_server_from_service(Host),
	ejabberd_odbc:sql_transaction(
        Host_Server,
		fun() ->
          ejabberd_odbc:sql_query_t(
	        [<<"insert into user_room(user_id, server, room_id, service, affiliation) values ('">>,
		    User, <<"', '">>, Server, <<"', '">>, Name, <<"', '">>, Host, <<"', '">>, 
			atom_to_list(Affilation), <<"');">>])
		end).

delete_member({User, Server}, {Name, Host}, Affilation) ->
	ejabberd_odbc:sql_transaction(
		Server,
		fun() ->
          ejabberd_odbc:sql_query_t(
	        [<<"delete from user_room where user_id = '">>, User, <<"' and server ='">>,
			Server, <<"' and room_id = '">>, Name, <<"' and service = '">>, Host,
			<<"' and affiliation = '">>, atom_to_list(Affilation), <<"';">>])
		end).

delete_member({User, Server}, {Name, Host}) ->
    ejabberd_odbc:sql_transaction(
        Server,
		fun() ->
          ejabberd_odbc:sql_query_t(
	        [<<"delete from user_room where user_id = '">>, User, <<"' and server ='">>,
		    Server, <<"' and room_id = '">>, Name, <<"' and service = '">>, Host, <<"';">>])
		end).


update_user({User, Server}, {Name, Host}, Affiliation) ->
	Host_Server = mod_muc_company:get_server_from_service(Host),
	ejabberd_odbc:sql_transaction(
		Host_Server, 
		fun() ->
          ejabberd_odbc:sql_query_t(
				[<<"update user_room set affiliation='">>, atom_to_list(Affiliation), 
				<<"' where user_id='">>, User, <<"'and server='">>, Server,
				<<"' and room_id ='">>, Name, <<"' and service ='">>, Host,<<"';">>])
		end).

%% Input: Server, User
%% Output: {{us = {U, S}}, {room = {room_id, service}}, affiliation}
get_user_rooms(User, Server) ->
    Result = ejabberd_odbc:sql_query(
        Server,
		[<<"select user_id, server, room_id, service, affiliation from user_room where user_id = '">>, 
		User, <<"' and server = '">>, Server, <<"';">>]),
	case Result of
        {selected, ?COLUMNS_USER_ROOM, []} ->
                [];
        {selected, ?COLUMNS_USER_ROOM, Res} ->
                convert_from_result(Res);
        _ ->
                error
    end.

%% Input: User, Server
%% Output: {roomid, service}
get_room_list_of_user(User, Server) ->
    Result = ejabberd_odbc:sql_query(
        Server,
        [<<"select room_id, service from user_room where user_id = '">>,
        User, <<"' and server = '">>, Server, <<"';">>]),
    case Result of
        {selected, [<<"room_id">>, <<"service">>], Res} ->
                Res;
        _ ->
                error
    end.

%% Input: Server, name of room, service
%% Output: {User, Server}
get_room_members(Server, {Name, Host}) ->
    Result = ejabberd_odbc:sql_query(
        Server,
		[<<"select user_id, server from user_room where room_id ='">>, 
		Name, <<"' and service ='">>, Host, <<"';">>]),
	case Result of
        {selected, [<<"user_id">>, <<"server">>], []} ->
					[];
        {selected, [<<"user_id">>, <<"server">>], Res} ->
					Res;
        _ ->
                error
    end.

%% Input: Server, name of room, service
%% Output: {User, Server}
get_all_room_members(Server, {Name, Host}) ->
    Result = ejabberd_odbc:sql_query(
        Server,
        [<<"select user_id, server, room_id, service, affiliation from user_room where room_id ='">>,
        Name, <<"' and service ='">>, Host, <<"';">>]),
    case Result of
        {selected, ?COLUMNS_USER_ROOM, []} ->
                    [];
        {selected, ?COLUMNS_USER_ROOM, Res} ->
                    convert_from_result(Res);
        _ ->
                error
    end.

remove_affiliations(Server, {Name, Host}) ->
	RoomMembers = get_all_room_members(Server, {Name, Host}),
	case RoomMembers of
		error -> 
				fail;
		_ ->
			ListMembers = lists:map(fun(R) -> 
                    delete_member(R#user_room.us, {Name, Host}, 
										R#user_room.affiliation)
            end, RoomMembers)
	end.

get_rooms_of_user({User, Server}) ->
	Result = ejabberd_odbc:sql_query(Server,
        [<<"select room_id, service from user_room where user_id ='">>,
        User, <<"' and server ='">>, Server, <<"';">>]),
    case Result of
        {selected, [<<"room_id">>, <<"service">>], []} ->
                    [];
        {selected, [<<"room_id">>, <<"service">>], Res} ->
                    Res;
        _ ->
                error
    end.

get_default_room_of_user({User, Server}) ->
	 Result = ejabberd_odbc:sql_query(Server,
        [<<"select ruser.user_id, ruser.server, ruser.room_id, ruser.service, ruser.affiliation ">>, 
		<<"from user_room as ruser, company_room as croom  where ruser.user_id ='">>,
        User, <<"' and ruser.server ='">>, Server, 
		<<"'and ruser.room_id = croom.room_id and croom.default_flag = 1;">>]),
    case Result of
        {selected, ?COLUMNS_USER_ROOM, []} ->
                    [];
        {selected, ?COLUMNS_USER_ROOM, Res} ->
                convert_from_result(Res);
        _ ->
                error
    end.

%%----------------------------------------------
%% TODO: Internal functions
%%----------------------------------------------
convert_from_result(BListUser) ->
	lists:map(fun({U, S, Name, Host, Affiliation}) -> 
				UserRoom = #user_room{us = {U, S}, room = {Name, Host}, 
					affiliation = binary_to_atom(Affiliation, utf8)}
				end, BListUser).


