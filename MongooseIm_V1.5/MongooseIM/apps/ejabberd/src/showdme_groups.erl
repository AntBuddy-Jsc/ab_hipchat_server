%% @author bokner
%% @doc @todo Add description to showdme_groups.


-module(showdme_groups).

%% ====================================================================
%% API functions
%% ====================================================================
-export([subscribe/4]).

-include("showdme.hrl").
-include("jlib.hrl").
-include("ejabberd.hrl").

subscribe(Host, From, To, #iq{sub_el = Query} = IQ) ->
  	GroupEl = xml:get_subtag(Query, <<"groups">>),
	#xmlel{children = GroupEls} = GroupEl,
	NewGroupSubscriptions = lists:foldl(fun(#xmlel{attrs = Attrs}, Acc) ->
								  [{xml:get_attr_s(<<"groupid">>, Attrs), 
									xml:get_attr_s(<<"content-types">>, Attrs),
									xml:get_attr_s(<<"event-types">>, Attrs)} | Acc];
								  (_, Acc) ->
					  				Acc
				  				end, [], GroupEls),
	lists:foreach(fun(#user_room{room = {GroupId, _Service}}) ->
									SubscriptionMsg = 
									  case lists:keyfind(GroupId, 1, NewGroupSubscriptions) of
									  	false ->
											set_group_subscription(Host, From, GroupId, <<>>, <<>>);
										{_GroupId, ContentTypes, EventTypes} ->
										  	set_group_subscription(Host, From, GroupId, ContentTypes, EventTypes)
									  end
				  end, mod_muc:get_user_rooms(From#jid.luser, From#jid.lserver)),
	
									  				 
    %%GroupSubscriptions = 
    Res = IQ#iq{type = result,
		sub_el = [#xmlel{name = <<"query">>,
			         attrs = [{<<"xmlns">>, ?NS_SHOWDME_GROUP_SUBSCRIBE}],
			         children = [#xmlel{name = <<"success">>}]
						}
				 ]
			   },
    ejabberd_router:route(To,
			  From,
			  jlib:iq_to_xml(Res)).
%% ====================================================================
%% Internal functions
%% ====================================================================
set_group_subscription(Host, From, GroupId, ContentTypes, EventTypes) ->
  ?DEBUG("Handling group subscription for ~p", [GroupId]),
  case mod_muc:room_to_pid({GroupId, Host}) of
			  {ok, Pid} ->
				gen_fsm:sync_send_all_state_event(Pid, {subscribe, From, ContentTypes, EventTypes});
			  Error ->
				Error
			end.

