-module(mod_active_status_odbc).

%% API for database user_active_room
-export([
		 add_status_user/2
		, insert_multi_records/2
		, update_status_user/2
		, update_inactive_user/2
		, get_timestamp_status_user/3
        , get_timestamp_status_user/4
		, delete_user/2
        ]).

-include("ejabberd.hrl").

%%CREATE TABLE user_active_room (
%%    id bigserial NOT NULL,
%%    userjid character varying(250) NOT NULL,
%%    server_name character varying(250) NOT NULL,
%%    object_jid character varying(250) NOT NULL,
%%    status_object smallint NOT NULL,
%%    update_timestamp timestamp without time zone default (os:timestamp() at time zone 'utc'),
%%    CONSTRAINT us_object PRIMARY KEY(id)
%%);

get_timestamp_status_user(Server, User, Server_User) ->
    Result = ejabberd_odbc:sql_query(
        Server,
        [<<"select object_jid, status_object, max(data_timestamp) from user_active_room ">>,
        <<" where userjid = '">>, User, <<"' and server_name = '">>, Server_User, 
        <<"' group by object_jid,status_object order by object_jid,status_object desc;">>]),
    case Result of
        {selected, [<<"object_jid">>, 
					<<"status_object">>, <<"max">>], Res} ->
				?DEBUG("AB#43: Res: ~p~n", [Res]),
				FRes = [{Object, {Status, Ts}} ||
							{Object, Status, Ts} <- Res],
				lists:map(fun(K) ->
						Values = proplists:get_all_values(K, FRes),
						case length(Values) of
							1 ->
								[Value] = Values,
								{K, Value};
							_ ->
								case lists:keyfind(<<"1">>, 1, Values) of
									{<<"1">>, Ts} ->
										{K, {<<"1">>, Ts}};
									_ ->
										{K, Values}
								end
						end
				end, proplists:get_keys(FRes));
        Err -> ?ERROR_MSG("AB#43: Err: ~p~n", [Err]), 
            []
    end.

get_timestamp_status_user(Server, User, Server_User, ObjectJID) ->
    Result = ejabberd_odbc:sql_query(
        Server,
        [<<"select 
                DISTINCT ON (status_object)
                status_object, data_timestamp 
            from user_active_room ">>,
        <<" where userjid = '">>, User, <<"' and server_name = '">>, 
        Server_User, <<"' and object_jid = '">>, ObjectJID,
        <<"' order by status_object, update_timestamp desc;">>]),
    case Result of
        {selected, [<<"status_object">>, <<"data_timestamp">>], []} ->
            [];
        {selected, [<<"status_object">>, <<"data_timestamp">>], Res} ->
            case lists:keyfind(<<"1">>, 1, Res) of
                {<<"1">>, Ts} ->
                        {<<"1">>, Ts};
                Any when is_list(Res)->
                        lists:last(Res);
                _ ->
                    Res
            end;
        Err -> ?ERROR_MSG("AB#43: Err: ~p~n", [Err]), 
            []
    end.

add_status_user(Server, {UserID, Server_User, ObjectID, Status, Timestamp}) ->
    ?DEBUG("#86: posgres: add_status_user: User: ~p, Server_User: ~p, ObjectID: ~p, 
        Status: ~p, Timestamp: ~p",[UserID, Server_User, ObjectID, Status, Timestamp]),
    ejabberd_odbc:sql_transaction(
        Server,
        fun() ->
          ejabberd_odbc:sql_query_t(
            [<<"insert into user_active_room(userjid, server_name, object_jid, status_object, update_timestamp, data_timestamp) 
			values ('">>, UserID, <<"', '">>, Server_User, <<"', '">>, ObjectID, <<"', ">>, 
			Status, <<", '">>, Timestamp, <<"', '">>, Timestamp, <<"');">>])
        end).

update_status_user(Server, {UserID, Server_User, ObjectID, Status, Timestamp}) ->
    ?DEBUG("#86: posgres: update_status_user: User: ~p, Server_User: ~p, ObjectID: ~p, 
        Status: ~p, Timestamp: ~p",[UserID, Server_User, ObjectID, Status, Timestamp]),
    Res = ejabberd_odbc:sql_transaction(
        Server,
        fun() ->
          ejabberd_odbc:sql_query_t(
            [<<"update user_active_room set status_object = ">>, Status, 
			<<", update_timestamp = '">>, Timestamp, <<"', data_timestamp = '">>, Timestamp,
			<<"' where userjid = '">>, UserID,
            <<"' and server_name = '">>, Server_User, <<"' and object_jid = '">>, ObjectID, <<"';">>])
        end),
    ?DEBUG("#86: posgres: update_status_user: Response: ~p",[Res]),
	case Res of
		{atomic, {updated, 0}} ->
			case get_info_user(Server, {UserID, Server_User, ObjectID}) of
					[] -> 
						add_status_user(Server, {UserID, Server_User, ObjectID, Status, Timestamp});
					Object ->
						Object		
			end;
		_ ->
			Res
	end.

get_info_user(Server, {UserID, Server_User, ObjectID}) ->
    Result = ejabberd_odbc:sql_query(Server,
        [<<"select id from user_active_room where userjid = '">>, UserID, 
        <<"' and server_name = '">>, Server_User, 
        <<"' and object_jid = '">>, ObjectID, <<"';">>]),
    %?INFO_MSG("get_info_user: Server: ~p; UserID: ~p; Server_User: ~p; ObjectID: ~p; Result: ~p~n", [Server, UserID, Server_User, ObjectID, Result]),
    case Result of
        [] ->
            [];
        {selected, [<<"id">>], Res} ->
            Res;
        Err ->
            ?ERROR_MSG("Err: ~p~n", [Err]), []
    end.

update_inactive_user(Server, {UserID, ServerUser, ObjectID, Timestamp}) ->
    Res =
    ejabberd_odbc:sql_transaction(
        Server,
        fun() ->
          ejabberd_odbc:sql_query_t(
            [<<"update user_active_room set status_object = 0, update_timestamp = '">>, Timestamp, 
            <<"', data_timestamp = '">>, Timestamp,
            <<"' where userjid = '">>, UserID, <<"' and server_name = '">>, ServerUser, 
            <<"' and object_jid = '">>, ObjectID, <<"' and status_object = 1;">>])
        end),
    case Res of
        {atomic, {updated, 0}} ->
            case get_info_user(Server, {UserID, ServerUser, ObjectID}) of
                    [] ->
                        add_status_user(Server, {UserID, ServerUser, ObjectID, <<"0">>, Timestamp});
                    Object ->
                        Object      
            end;
        _ ->
            Res
    end.

delete_user(Server, {UserID, Server_User, ObjectID}) ->
	ejabberd_odbc:sql_transaction(
        Server,
        fun() ->
          ejabberd_odbc:sql_query_t(
            [<<"delete from user_active_room where userjid = '">>, UserID, <<"' and server_name = '">>, Server_User, 
			<<"' and object_jid = '">>, ObjectID, <<"';">>])
        end).

insert_multi_records(Server, Records) ->
	ejabberd_odbc:sql_transaction(
        Server,
        fun() ->
          ejabberd_odbc:sql_query_t(
            [<<"insert into user_active_room(userjid, server_name, object_jid, status_object, update_timestamp, data_timestamp) 
            values ">>, Records, <<";">>])
        end).
