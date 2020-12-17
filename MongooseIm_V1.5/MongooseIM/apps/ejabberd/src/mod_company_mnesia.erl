-module(mod_company_mnesia).

-export([start/2, stop/1]).

%% API for user_room
-export([add_new_user/3,
        get_user_rooms/2,
        get_room_members/2,
        remove_affiliations/2,
		get_rooms_of_user/1]).

-include("showdme.hrl").
-include("ejabberd.hrl").
-define(BY_ROOM_MATCH(Name, Host),  #user_room{room = {Name, Host}, _ = '_'}).

start(_Host, _Opts) ->
	ok.

stop(_Host) ->
	ok.

add_new_user({U, S}, {Name, Host}, Affiliation) ->
	mnesia:dirty_write(#user_room{us = {U, S}, 
			room = {Name, Host}, affiliation = Affiliation}).

get_user_rooms(User, Server) ->
	ets:lookup(user_room, {User, Server}).

get_room_members(_Server, {RoomName, RoomService}) ->
	%% The spec was generated from:
	%% ets:fun2ms(fun({user_room, US, {Name, Service}, _}) when Name =:= RoomName andalso Service =:= RoomService -> US end).
	UserMatch = [{#user_room{us = '$1',room = {'$2','$3'},affiliation = '_'},
	[{'andalso',{'==','$2', RoomName},
              {'==','$3', RoomService}}],
	['$1']}],
		ets:select(user_room, UserMatch).

remove_affiliations(_Server, {Name, Host}) ->
  lists:foreach(fun(A) ->
                      mnesia:dirty_delete_object(A)
              end, mnesia:dirty_match_object(?BY_ROOM_MATCH(Name, Host))).


%% Input :  {UserId, <<"htklabs.com">>}
%% Output :  List of rooms of member 
get_rooms_of_user(UserKey) ->
    Rooms = mnesia:dirty_read(user_room, UserKey),
    lists:map(fun(#user_room{room= Room}) ->
        Room
    end, Rooms).
