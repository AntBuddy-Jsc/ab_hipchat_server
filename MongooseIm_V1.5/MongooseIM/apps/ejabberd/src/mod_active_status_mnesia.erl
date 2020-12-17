-module (mod_active_status_mnesia).

-include("ejabberd.hrl").
-include("mod_antbuddy_stanza.hrl").

-export([start/0
		, create_user_resource_active/2
		, delete_user_resource_active/2
		, get_user_resource_active/1
		, get_user_resource_object/3
		]).

start() ->
    mnesia:create_table(user_active,
				[{attributes, record_info(fields, user_active)},
				{index, [#user_active.us_object]},
				{ram_copies, [node()]}]),
	mnesia:add_table_copy(user_active, node(), ram_copies).

create_user_resource_active({U, S, R}, Object) ->
    mnesia:sync_dirty(fun() ->
                              mnesia:write(#user_active{
                              			usr = {U, S, R},
                              			object = Object,
                              			us_object = {U, S, Object}})
                      end).

delete_user_resource_active({U, S, R}, Object) ->
    mnesia:sync_dirty(fun() ->
                              mnesia:delete_object(#user_active{
                              			usr = {U, S, R},
                              			object = Object,
                              			us_object = {U, S, Object}})
                      end).

get_user_resource_object(User, Server, Object) ->
    case mnesia:dirty_index_read(user_active, 
    					{User, Server, Object}, 
    					#user_active.us_object) of 
    	[] ->
    		[];
    	Result when is_list(Result) ->
    		lists:map(fun(#user_active{usr = {_, _, R}}) -> 
    						R end, Result);
    	_ ->
    		[]
	end.

get_user_resource_active({User, Server, Resource}) ->
	case mnesia:dirty_read(user_active, {User, Server, Resource}) of
		[#user_active{object = Object}] ->
			Object;
		_ ->
			[]
	end.
