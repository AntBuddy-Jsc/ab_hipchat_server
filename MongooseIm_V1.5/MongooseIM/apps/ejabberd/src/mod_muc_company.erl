-module(mod_muc_company).
-behavior(gen_mod).

%% gen_mod
-export([start/2, stop/1]).

%% API for user_room
-export([add_new_user/3,
		get_user_rooms/2,
		get_room_members/2,
		remove_affiliations/2,
		get_rooms_of_user/1,
		get_default_room_of_user/1,
		get_server_from_service/1,
		update_user/3,
        delete_user/2]).

-include("ejabberd.hrl").
-include("showdme.hrl").
-include("jlib.hrl").

start(Host, Opt) ->
	M = database_module(Host),
	?DEBUG("AB#24 method access: ~p~n", [M]),
	M:start(Host, Opt),
	ok.

stop(Host) ->
	M = database_module(Host),
	M:stop(Host).

add_new_user({User, Server}, {Name, Host}, Affiliation) ->
	M = database_module(get_server_from_service(Host)),
	M:add_new_user({User, Server}, {Name, Host}, Affiliation).

update_user({User, Server},{Name, Host},  Affiliation) ->
	 M = database_module(get_server_from_service(Host)),
	 M:update_user({User, Server}, {Name, Host}, Affiliation).

get_user_rooms(User, Server) ->
	M = database_module(Server),
    M:get_user_rooms(User, Server).

get_room_members(Name, Host) ->
	Server = get_server_from_service(Host),
	M = database_module(Server),
    M:get_room_members(Server, {Name, Host}).

remove_affiliations(Name, Host) ->
    Server = get_server_from_service(Host),
	M = database_module(Server),
    M:remove_affiliations(Server, {Name, Host}).

get_rooms_of_user({User, Server}) ->
	M = database_module(Server),
	M:get_rooms_of_user({User, Server}).

get_default_room_of_user({User, Server}) ->
	M = database_module(Server),
	M:get_default_room_of_user({User, Server}).

delete_user({User, Server}, {Name, Host}) ->
	M = database_module(Server),
	M:delete_member({User, Server}, {Name, Host}).
%%--------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------
%% check which database will be chosen to access user_room
database_module(Server) ->
	LServer = jlib:nameprep(Server),
	ejabberd_config:get_local_option({odbc_server, LServer}),
	Method = case ejabberd_config:get_local_option({odbc_server, LServer}) of
		{DB_Type, _, _, _, _} ->  "_odbc";
		undefined ->	"_mnesia"
	end,
	list_to_atom("mod_company" ++ Method).

get_server_from_service(Service) ->
	BServer = binary:split(Service, [<<"conference.">>], []),
	lists:last(BServer).
