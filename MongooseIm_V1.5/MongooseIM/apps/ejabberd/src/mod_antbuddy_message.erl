-module(mod_antbuddy_message).

-behaviour(gen_mod).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_antbuddy_stanza.hrl").

%% gen_mod 
-export([start/2, stop/1]).

%% functions
-export([push_notification/3
        , push_notification_invite_room/3
        , check_always_push_message/1
        ]).

-define(WORKER, mod_antbuddy_message_worker).
-define(SUBTYPES, [<<"adduser">>
                    , <<"remove">>
                    , <<"bot-gitlab">>
                    , <<"welcome">>]).

start(Host, Opts) ->
    %% hook for push notification
    ConferenceHost = gen_mod:get_opt_host(Host, Opts, <<"conference.@HOST@">>),
    ejabberd_hooks:add(push_notification, Host, ?MODULE, push_notification, 75),
    ejabberd_hooks:add(push_notification_invite_room, ConferenceHost, ?MODULE, push_notification_invite_room, 75),
    
    %%create worker pool for push notification
    manager_pool:ensure_started(worker_pool),
    manager_pool:start_link(?WORKER, Host, Opts).

stop(Host) ->
    %% remove hook for push notification
    ConferenceHost = <<"conference.", Host/binary>>,
    ejabberd_hooks:delete(push_notification, Host, ?MODULE, push_notification, 75),
    ejabberd_hooks:delete(push_notification_invite_room, ConferenceHost, ?MODULE, push_notification_invite_room, 75).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %% Analysis type of message
    %% Check user is active or inactive (user is active when user has at least an online resource)
    %% Send push notification to Web API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
push_notification(From, To, Stanza) ->
    Type = xml:get_tag_attr_s(<<"type">>, Stanza),
    case allow_push_notification(Type, To, Stanza) of
        allow ->
            mod_antbuddy_message_worker:push_normal(From#jid.lserver, {push_normal, [From, To, Stanza]});
            % manager_pool:wcast(From#jid.lserver, ?WORKER, {push_normal, [From, To, Stanza]}, next_worker);
        _ ->
            not_allow_to_push        
    end.

push_notification_invite_room(From, To, RoomJID) ->
    manager_pool:wcast(From#jid.lserver, ?WORKER, {push_invite, [From, To, RoomJID]}, next_worker).

check_always_push_message(Stanza) ->
	is_buzz_message(Stanza) 
		orelse is_kite_request(Stanza).

allow_push_notification(Type, To, Stanza) ->
    case check_always_push_message(Stanza) of
        true ->
            allow;
        false ->
            case is_default_room(Type, To#jid.luser) of
                false ->
                    case is_message_to_push_notification(Stanza) of
                        true ->
                            allow;
                        false ->
                            false
                    end;
                true ->
                    false
            end
    end.

is_buzz_message(Stanza) ->
    case xml:get_subtag(Stanza, ?EVENT) of
        false ->
            false;
        Event ->
            case xml:get_tag_attr_s(<<"type">>, Event) of
                <<"buzz">> ->
                    true;
                _ ->
                    false
            end
    end,
    %% disable feature push everytime buzz message
    false.

is_kite_request(Stanza) ->
	case xml:get_tag_attr_s(<<"subtype">>, Stanza) of
		<<"KITE-REQUEST">> -> true;
		_ -> false
	end.
	
is_message_to_push_notification(Stanza) ->
    Subtype = xml:get_tag_attr_s(<<"subtype">>, Stanza),
    case (xml:get_subtag(Stanza, <<"oembed">>) == false) 
        andalso (xml:get_subtag(Stanza, ?EVENT) == false)
        andalso (xml:get_subtag_cdata(Stanza, <<"body">>) /= <<>>)
        andalso (lists:member(Subtype, ?SUBTYPES) == false)
        of
            true ->
                true;
            _ ->
                false
    end.

is_default_room(<<"groupchat">>, Receiver) ->
    case binary:split(Receiver, <<"_">>) of
        [Receiver] -> true;
        _ -> false
    end;
is_default_room(_, _) ->
    false.
