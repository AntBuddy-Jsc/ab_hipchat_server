%%%----------------------------------------------------------------------
%%% File    : mod_muc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC support (XEP-0045)
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(gen_mod).

%% API
-export([start_link/2,
         start/2,
         stop/1,
         room_destroyed/4,
         store_room/3,
         restore_room/2,
         forget_room/2,
         create_instant_room/5,
         process_iq_disco_items/4,
         broadcast_service_message/2,
         can_use_nick/3,
         room_jid_to_pid/1,
		 room_jid_to_pid/2,
	 	 delete_user_rooms/2, 
		 store_muc_room/4,
		 save_company_room/3,
		 store_affiliations/5,
		 check_room_default/2,
		 store_affiliations_to_default_room/5,
         register_room/3
		]).

-export([migrate_data/0]).
-export([restart_room/2, get_pid_room/2]).

%% Additions to include rooms the user has affiliation in to the roster
-export([get_user_rooms/2, get_user_rooms/3, get_room_roster/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% Internal exports
-export([route/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("showdme.hrl").


-export_type([access/0,
             room/0,
             nick/0,
             packet/0,
             role/0,
             affiliation/0
            ]).

-type role() :: moderator | participant | visitor | none.
-type affiliation() :: admin | owner | member | outcast | none.
-type room() :: binary().
-type nick() :: binary().
-type room_host() :: ejabberd:literal_jid(). % binary() too
-type packet() :: jlib:xmlel().
-type from_to_packet() ::
        {From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: packet()}.
-type access() :: {_AccessRoute, _AccessCreate, _AccessAdmin, _AccessPersistent}.

-record(muc_room, {name_host    :: room_host(),
                   opts         :: list()
                  }).

-record(muc_online_room, {name_host :: room_host(),
                          pid       :: pid()
                         }).
-type muc_online_room() :: #muc_online_room{}.

-record(muc_registered, {us_host    :: ejabberd:literal_jid(),
                         nick       :: nick
                        }).

-record(state, {host                :: ejabberd:server(),
                server_host         :: ejabberd:literal_jid(),
                access,
                history_size        :: integer(),
                default_room_opts   :: list(),
                room_shaper         :: shaper:shaper()
              }).
-type state() :: #state{}.

-define(PROCNAME, ejabberd_mod_muc).
-define(ROOM_NAME(Name, Service), <<Name/binary, "@", Service/binary>>).
-define(BY_ROOM_MATCH(Name, Host),  #user_room{room = {Name, Host}, _ = '_'}).

-define(DEFAULT_OPTIONS, [
               {allow_change_subj,false},
               {allow_user_invites,false},
               {allow_private_messages,false},
               {public,true},
               {persistent, true},
               {anonymous_domain, true}
              ]).
-define(ROOM_ACCESS_PARAMS, {muc, muc_create, muc_admin, muc_create}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link(ejabberd:server(), list())
            -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).


-spec start(ejabberd:server(),_) -> {'error',_}
            | {'ok','undefined' | pid()} | {'ok','undefined' | pid(),_}.
start(Host, Opts) ->
    start_supervisor(Host),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
        {Proc,
         {?MODULE, start_link, [Host, Opts]},
         temporary,
         1000,
         worker,
         [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).


-spec stop(ejabberd:server()) -> 'ok'
    | {'error','not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop(Host) ->
    stop_supervisor(Host),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    supervisor:delete_child(ejabberd_sup, Proc).


%% @doc This function is called by a room in three situations:
%% A) The owner of the room destroyed it
%% B) The only participant of a temporary room leaves it
%% C) mod_muc:stop was called, and each room is being terminated
%%    In this case, the mod_muc process died before the room processes
%%    So the message sending must be catched
-spec room_destroyed(ejabberd:server(), room(), pid(),
                     ejabberd:server()) -> 'ok'.
room_destroyed(Host, Room, Pid, ServerHost) ->
    catch gen_mod:get_module_proc(ServerHost, ?PROCNAME) !
        {room_destroyed, {Room, Host}, Pid},
    ok.


%% @doc Create a room.
%% If Opts = default, the default room options are used.
%% Else use the passed options as defined in mod_muc_room.
-spec create_instant_room(ejabberd:server(), Name :: room(),
    From :: ejabberd:jid(), Nick :: nick(), Opts :: list()) -> any().
create_instant_room(Host, Name, From, Nick, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, {create_instant, Name, From, Nick, Opts}).


-spec store_room(ejabberd:server(), room(), list())
            -> {'aborted',_} | {'atomic',_}.
store_room(Host, Name, Opts) ->
    F = fun() ->
		mnesia:write(#muc_room{name_host = {Name, Host},
				       opts = Opts})
		%% Store affiliations for the access by user JID
		%Affiliations = proplists:get_value(affiliations, Opts),
		%store_affiliations(Name, Host, Affiliations)
	end,
    mnesia:transaction(F).


-spec restore_room(ejabberd:server(), room())
                                    -> 'error' | 'undefined' | [any()].
restore_room(Host, Name) ->
    case catch mnesia:dirty_read(muc_room, {Name, Host}) of
        [#muc_room{opts = Opts}] ->
            Opts;
        _ ->
            error
    end.

restart_room(RoomID, RoomHost) ->
    ServerHost =
    case binary:split(RoomHost, [<<".">>]) of
      [_, Host] -> Host;
      [Other] -> Other;
      _ -> []
    end,
    Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
    RoomPid = get_pid_room(RoomID, RoomHost),
    case is_pid(RoomPid) of
      true ->
        ?INFO_MSG("Terminate room pid: ~p; self() ~p~n", [RoomPid, self()]),
        mod_muc:room_destroyed(RoomHost, RoomID, RoomPid, ServerHost),
        supervisor:terminate_child(Supervisor, RoomPid),
        supervisor:delete_child(Supervisor, RoomPid);
        %ok;
      _ -> ok
    end,
    mnesia:dirty_delete(muc_online_room, {RoomID, RoomHost}),
    Options =
    case mnesia:dirty_read(muc_room, {RoomID, RoomHost}) of
        [{_, _, Res}] -> Res;
        _ -> ?DEFAULT_OPTIONS
    end,
    {ok, Pid} = mod_muc_room:start(RoomHost,ServerHost,?ROOM_ACCESS_PARAMS,RoomID,0,none,Options),
    mod_muc:register_room(RoomHost, RoomID, Pid).

get_pid_room(RoomID, RoomHost) ->
    case mnesia:dirty_read(muc_online_room, {RoomID, RoomHost}) of
        [{muc_online_room, {_, _}, Pid}] -> Pid;
        _ -> nonexistence
    end.

-spec forget_room(ejabberd:server(), room()) -> 'ok'.
forget_room(Host, Name) ->
    F = fun() ->
                mnesia:delete({muc_room, {Name, Host}})
        end,
    mnesia:transaction(F),
	%% AB#19: delete all info related to room was destroyed
	remove_affiliations(Name, Host),
    ejabberd_hooks:run(forget_room, Host, [Host, Name]),
    ok.

%% Persisten affiliations
%% AB#25
store_affiliations(Name, Host, _Affiliations, LJID, none) ->
	JID = jlib:make_jid(LJID),
    mod_muc_company:delete_user({JID#jid.luser, JID#jid.lserver}, {Name, Host});

store_affiliations(Name, Host, Affils, LJID, Affiliation) ->
   JID = jlib:make_jid(LJID),
   case dict:find(LJID, Affils) of
	{ok, _} ->
		 mod_muc_company:update_user({JID#jid.luser, JID#jid.lserver}, {Name, Host}, Affiliation) ;
	_ ->
		 mod_muc_company:add_new_user({JID#jid.luser, JID#jid.lserver}, {Name, Host}, Affiliation)
	end.

store_affiliations_to_default_room(Host, Affils, LJID, Affiliation, RoomList) ->
	lists:foreach(fun({Room}) ->
				store_affiliations(Room, Host, Affils, LJID, Affiliation)
			end, RoomList).	

remove_affiliations(Name, Host) ->
  %% AB#22
  %lists:foreach(fun(A) ->
  %                    mnesia:dirty_delete_object(A)
  %            end, mnesia:dirty_match_object(?BY_ROOM_MATCH(Name, Host))).
  mod_muc_company:remove_affiliations(Name, Host).


process_iq_disco_items(Host, From, To, #iq{lang = Lang} = IQ) ->
    Rsm = jlib:rsm_decode(IQ),
    Res = IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"query">>,
                                 attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS}],
                                 children = iq_disco_items(Host, From, Lang, Rsm)}]},
    ejabberd_router:route(To,
                          From,
                          jlib:iq_to_xml(Res)).

%%can_use_nick(_Host, _JID, <<>>) ->
%%    false;

can_use_nick(_Host, JID, Nick) ->
  ok, JID#jid.luser == Nick.

%%     {LUser, LServer, _} = jlib:jid_tolower(JID),
%%     LUS = {LUser, LServer},
%%     case catch mnesia:dirty_select(
%% 		 muc_registered,
%% 		 [{#muc_registered{us_host = '$1',
%% 				   nick = Nick,
%% 				   _ = '_'},
%% 		   [{'==', {element, 2, '$1'}, Host}],
%% 		   ['$_']}]) of
%% 	{'EXIT', _Reason} ->
%% 	    true;
%% 	[] ->
%% 	    true;
%% 	[#muc_registered{us_host = {U, _Host}}] ->
%% 	    U == LUS
%%     end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec init([ejabberd:server() | list(),...]) -> {'ok',state()}.
init([Host, Opts]) ->
    mnesia:create_table(muc_room,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, muc_room)}]),
    mnesia:create_table(muc_registered,
                        [{disc_copies, [node()]},
                         {attributes, record_info(fields, muc_registered)}]),
    mnesia:create_table(muc_online_room,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, muc_online_room)}]),

	%% Table to support users per room
	mnesia:create_table(user_room,
			[{ram_copies, [node()]},
			 {type, bag},
			 {index, [#user_room.room]},
			 {attributes, record_info(fields, user_room)}]),		
    mnesia:add_table_copy(user_room, node(), ram_copies),
    mnesia:add_table_copy(muc_online_room, node(), ram_copies),
    mnesia:add_table_copy(muc_room, node(), disc_copies),
    mnesia:add_table_copy(muc_registered, node(), disc_copies),
    catch ets:new(muc_online_users, [bag, named_table, public, {keypos, 2}]),
    muc_util:new_track_org(),
    MyHost = gen_mod:get_opt_host(Host, Opts, <<"conference.@HOST@">>),
    update_tables(MyHost),
    clean_table_from_bad_node(node(), MyHost),
    mnesia:add_table_index(muc_registered, nick),
    mnesia:subscribe(system),
    Access = gen_mod:get_opt(access, Opts, all),
    AccessCreate = gen_mod:get_opt(access_create, Opts, all),
    AccessAdmin = gen_mod:get_opt(access_admin, Opts, none),
    AccessPersistent = gen_mod:get_opt(access_persistent, Opts, all),
    HistorySize = gen_mod:get_opt(history_size, Opts, 20),
    DefRoomOpts = gen_mod:get_opt(default_room_options, Opts, []),
    RoomShaper = gen_mod:get_opt(room_shaper, Opts, none),

    State = #state{host = MyHost,
            server_host = Host,
            access = {Access, AccessCreate, AccessAdmin, AccessPersistent},
            default_room_opts = DefRoomOpts,
            history_size = HistorySize,
            room_shaper = RoomShaper},

    F = fun(From, To, Packet) ->
            mod_muc:route({From, To, Packet}, State)
        end,
    ejabberd_router:register_route(MyHost, {apply_fun, F}),

	%% Add roster hook for "room roster"
	ejabberd_hooks:add(roster_get, Host, ?MODULE, get_room_roster, 100),

    load_permanent_rooms(MyHost, Host,
                         {Access, AccessCreate, AccessAdmin, AccessPersistent},
                         HistorySize,
                         RoomShaper),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({create_instant, Room, From, Nick, Opts},
            _From,
            #state{host = Host,
                   server_host = ServerHost,
                   access = Access,
                   default_room_opts = DefOpts,
                   history_size = HistorySize,
                   room_shaper = RoomShaper} = State) ->
    ?DEBUG("MUC: create new room '~s'~n", [Room]),
    NewOpts = case Opts of
                  default -> DefOpts;
                  _ -> Opts
              end,
    {ok, Pid} = mod_muc_room:start(
                  Host, ServerHost, Access,
                  Room, HistorySize,
                  RoomShaper, From,
          Nick, [{instant, true}|NewOpts]),
    register_room(Host, Room, Pid),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({route, From, To, Packet}, State) ->
    case catch route({From, To, Packet}, State) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p", [Reason]);
        _ ->
            ok
    end,
    {noreply, State};
handle_info({room_destroyed, RoomHost, Pid}, State) ->
    F = fun() ->
                mnesia:delete_object(#muc_online_room{name_host = RoomHost,
                                                      pid = Pid})
        end,
    mnesia:transaction(F),
    {noreply, State};
handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    ejabberd_router:unregister_route(State#state.host),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec start_supervisor(ejabberd:server()) -> {'error',_}
                                           | {'ok','undefined' | pid()}
                                           | {'ok','undefined' | pid(),_}.
start_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_muc_sup),
    ChildSpec =
        {Proc,
         {ejabberd_tmp_sup, start_link,
          [Proc, mod_muc_room]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_sup, ChildSpec).


-spec stop_supervisor(ejabberd:server()) -> 'ok'
    | {'error','not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_muc_sup),
    supervisor:terminate_child(ejabberd_sup, Proc),
    supervisor:delete_child(ejabberd_sup, Proc).


-spec route({From :: ejabberd:jid(),
    To :: ejabberd:simple_jid() | ejabberd:jid(), Packet :: any()}, state())
            -> 'ok' | pid().
route(Routed, State) ->
    route_by_privilege(Routed, State).


-spec route_by_privilege({From :: ejabberd:jid(),
    To :: ejabberd:simple_jid() | ejabberd:jid(), Packet :: any()},
        state()) -> 'ok' | pid().
route_by_privilege({From, To, Packet} = Routed,
                   #state{access={AccessRoute,_,_,_},
                          server_host=ServerHost} = State) ->
    case acl:match_rule(ServerHost, AccessRoute, From) of
        allow ->
            {Room, _, _} = jlib:jid_tolower(To),
            route_to_room(Room, Routed, State);
        _ ->
            #xmlel{attrs = Attrs} = Packet,
            Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
            ErrText = <<"Access denied by service policy">>,
            Err = jlib:make_error_reply(Packet,
                                        ?ERRT_FORBIDDEN(Lang, ErrText)),
            ejabberd_router:route_error(To, From, Err, Packet)
    end.


-spec route_to_room(room(), from_to_packet(), state()) -> 'ok' | pid().
route_to_room(<<>>, {_,To,_} = Routed, State) ->
    {_, _, Nick} = jlib:jid_tolower(To),
    route_by_nick(Nick, Routed, State);
route_to_room(Room, {From,To,Packet} = Routed, #state{host=Host} = State) ->
    case mnesia:dirty_read(muc_online_room, {Room, Host}) of
        [] ->
            route_to_nonexistent_room(Room, Routed, State);
        [R] ->
            Pid = R#muc_online_room.pid,
            ?DEBUG("MUC: send to process ~p~n", [Pid]),
            {_, _, Nick} = jlib:jid_tolower(To),
            mod_muc_room:route(Pid, From, Nick, Packet),
            ok
    end.


-spec route_to_nonexistent_room(room(), from_to_packet(), state()) -> 'ok'.
route_to_nonexistent_room(Room, {From, To, Packet},
                          #state{host=Host} = State) ->
    #xmlel{name = Name, attrs = Attrs} = Packet,
    Type = xml:get_attr_s(<<"type">>, Attrs),
    case {Name, Type} of
        {<<"presence">>, <<>>} ->
            ServerHost = State#state.server_host,
            Access = State#state.access,
            {_, AccessCreate, _, _} = Access,
            case check_user_can_create_room(ServerHost, AccessCreate,
                                            From, Room) of
                true ->
                    HistorySize = State#state.history_size,
                    RoomShaper  = State#state.room_shaper,
                    DefRoomOpts = State#state.default_room_opts,
					DefaultFlag = get_default_value(Packet, Room), 
                    {_, _, Nick} = jlib:jid_tolower(To),
                    {ok, Pid} = start_new_room(Host, ServerHost, Access, Room,
                                               HistorySize, RoomShaper, From,
                                               Nick, DefRoomOpts, DefaultFlag),
                    register_room(Host, Room, Pid),
                    mod_muc_room:route(Pid, From, Nick, Packet),
					%% AB#23
					Pid ! {save_company_room, ServerHost, Room, DefaultFlag},
                    ok;
                false ->
                    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
                    ErrText = <<"Room creation is denied by service policy">>,
                    Err = jlib:make_error_reply(
                            Packet, ?ERRT_NOT_ALLOWED(Lang, ErrText)),
                    ejabberd_router:route(To, From, Err)
            end;
        _ ->
            Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
            ErrText = <<"Conference room does not exist">>,
            Err = jlib:make_error_reply(
                    Packet, ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
            ejabberd_router:route(To, From, Err)
    end.


-spec route_by_nick(room(), from_to_packet(), state()) -> 'ok' | pid().
route_by_nick(<<>>, {_,_,Packet} = Routed, State) ->
    #xmlel{name = Name} = Packet,
    route_by_type(Name, Routed, State);
route_by_nick(_Nick, {From, To, Packet}, _State) ->
    #xmlel{attrs = Attrs} = Packet,
    case xml:get_attr_s(<<"type">>, Attrs) of
        <<"error">> ->
            ok;
        <<"result">> ->
            ok;
        _ ->
            Err = jlib:make_error_reply(Packet, ?ERR_ITEM_NOT_FOUND),
            ejabberd_router:route(To, From, Err)
    end.


-spec route_by_type(binary(), from_to_packet(), state()) -> 'ok' | pid().
route_by_type(<<"iq">>, {From, To, Packet}, #state{host = Host} = State) ->
    ServerHost = State#state.server_host,
    case jlib:iq_query_info(Packet) of
        #iq{type = get, xmlns = ?NS_DISCO_INFO = XMLNS, lang = Lang} = IQ ->
            Info = ejabberd_hooks:run_fold(disco_info, ServerHost, [],
                                           [ServerHost, ?MODULE, "", Lang]),
            Res = IQ#iq{type = result,
                        sub_el = [#xmlel{name = <<"query">>,
                                         attrs = [{<<"xmlns">>, XMLNS}],
                                         children = iq_disco_info(Lang) ++ Info}]},
            ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{type = get, xmlns = ?NS_DISCO_ITEMS} = IQ ->
            spawn(?MODULE, process_iq_disco_items, [Host, From, To, IQ]);
	
	#iq{type = set, xmlns = ?NS_SHOWDME_GROUP_SUBSCRIBE} = IQ ->
	    spawn(showdme_groups, subscribe, [Host, From, To, IQ]);

        #iq{type = get, xmlns = ?NS_REGISTER = XMLNS, lang = Lang} = IQ ->
            Res = IQ#iq{type = result,
                        sub_el = [#xmlel{name = <<"query">>,
                                         attrs = [{<<"xmlns">>, XMLNS}],
                                         children = iq_get_register_info(Host, From, Lang)}]},
            ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{type = set,
            xmlns = ?NS_REGISTER = XMLNS,
            lang = Lang,
            sub_el = SubEl} = IQ ->
            case process_iq_register_set(Host, From, SubEl, Lang) of
                {result, IQRes} ->
                    Res = IQ#iq{type = result,
                                sub_el = [#xmlel{name = <<"query">>,
                                                 attrs = [{<<"xmlns">>, XMLNS}],
                                                 children = IQRes}]},
                    ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
                {error, Error} ->
                    Err = jlib:make_error_reply(Packet, Error),
                    ejabberd_router:route(To, From, Err)
            end;
        #iq{type = get, xmlns = ?NS_VCARD = XMLNS, lang = Lang} = IQ ->
            Res = IQ#iq{type = result,
                        sub_el = [#xmlel{name = <<"vCard">>,
                                         attrs = [{<<"xmlns">>, XMLNS}],
                                         children = iq_get_vcard(Lang)}]},
            ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{type = get, xmlns = ?NS_MUC_UNIQUE} = IQ ->
           Res = IQ#iq{type = result,
                       sub_el = [#xmlel{name = <<"unique">>,
                                        attrs = [{<<"xmlns">>, ?NS_MUC_UNIQUE}],
                                        children = [iq_get_unique(From)]}]},
           ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{} ->
            Err = jlib:make_error_reply(Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
            ejabberd_router:route(To, From, Err);
        _ ->
            ok
    end;
route_by_type(<<"message">>, {From, To, Packet},
              #state{host = Host, server_host = ServerHost,
                     access = {_,_,AccessAdmin,_}}) ->
    #xmlel{attrs = Attrs} = Packet,
    case xml:get_attr_s(<<"type">>, Attrs) of
        <<"error">> ->
            ok;
        _ ->
            case acl:match_rule(ServerHost, AccessAdmin, From) of
                allow ->
                    Msg = xml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),
                    broadcast_service_message(Host, Msg);
                _ ->
                    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
                    ErrText = <<"Only service administrators are allowed to send service messages">>,
                    Err = ?ERRT_FORBIDDEN(Lang, ErrText),
                    ErrorReply = jlib:make_error_reply(Packet, Err),
                    ejabberd_router:route(To, From, ErrorReply)
            end
    end;
route_by_type(<<"presence">>, _Routed, _State) ->
    ok.


-spec check_user_can_create_room('global' | ejabberd:server(),
        'allow' | atom(), ejabberd:jid(), room()) -> boolean().
check_user_can_create_room(ServerHost, AccessCreate, From, RoomID) ->
    %case acl:match_rule(ServerHost, AccessCreate, From) of
	case check_acl_create(ServerHost, AccessCreate, From, RoomID) of
        allow ->
            (size(RoomID) =< gen_mod:get_module_opt(ServerHost, mod_muc,
                                                    max_room_id, infinite));
        _ ->
            false
    end.

check_acl_create(ServerHost, AccessCreate, From, RoomID) ->
	case mod_muc_room:check_bot_company(From#jid.luser, RoomID)  of
			allow -> allow;
			not_bot_company -> none;
			_ -> 
				case acl:match_rule(ServerHost, AccessCreate, From) of
					allow -> allow;
					_ -> none
				end
	end.

-spec load_permanent_rooms(Host :: ejabberd:server(), Srv :: ejabberd:server(),
        Access :: access(), HistorySize :: 'undefined' | integer(),
        RoomShaper :: shaper:shaper()) -> 'ok'.
load_permanent_rooms(Host, ServerHost, Access, HistorySize, RoomShaper) ->
    case catch mnesia:dirty_select(
                 muc_room, [{#muc_room{name_host = {'_', Host}, _ = '_'},
                             [],
                             ['$_']}]) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("~p", [Reason]),
            ok;
        Rs ->
            lists:foreach(
              fun(R) ->
                      {Room, Host} = R#muc_room.name_host,
                      case mnesia:dirty_read(muc_online_room, {Room, Host}) of
                          [] ->
                              {ok, Pid} = mod_muc_room:start(
                                            Host,
                                            ServerHost,
                                            Access,
                                            Room,
                                            HistorySize,
                                            RoomShaper,
                                            R#muc_room.opts),
				%% Boris Okner: Add to "rooms per user table"
				%%  store_affiliations(Room, Host, proplists:get_value(affiliations, R#muc_room.opts)),
                              register_room(Host, Room, Pid);
                          _ ->
                              ok
                      end
              end, Rs)
    end.


-spec start_new_room(Host :: 'undefined' | ejabberd:server(),
        Srv :: ejabberd:server(), Access :: access(), room(),
        HistorySize :: 'undefined' | integer(), RoomShaper :: shaper:shaper(),
        From :: ejabberd:jid(), nick(), DefRoomOpts :: 'undefined' | [any()])
            -> {'error',_}
             | {'ok','undefined' | pid()}
             | {'ok','undefined' | pid(),_}.
start_new_room(Host, ServerHost, Access, Room,
               HistorySize, RoomShaper, From,
               Nick, DefRoomOpts) ->
    case mnesia:dirty_read(muc_room, {Room, Host}) of
        [] ->
            ?DEBUG("MUC: open new room '~s'~n", [Room]),
            mod_muc_room:start(Host, ServerHost, Access,
                               Room, HistorySize,
                               RoomShaper, From,
                               Nick, DefRoomOpts);
        [#muc_room{opts = Opts}|_] ->
            ?DEBUG("MUC: restore room '~s'~n", [Room]),
            mod_muc_room:start(Host, ServerHost, Access,
                               Room, HistorySize,
                               RoomShaper, Opts)
    end.

start_new_room(Host, ServerHost, Access, Room,
               HistorySize, RoomShaper, From,
               Nick, DefRoomOpts, DefaultFlag) ->
    case mnesia:dirty_read(muc_room, {Room, Host}) of
        [] ->
            ?DEBUG("MUC: open new room '~s'~n", [Room]),
            mod_muc_room:start(Host, ServerHost, Access,
                               Room, HistorySize,
                               RoomShaper, From,
                               Nick, DefRoomOpts, DefaultFlag);
        [#muc_room{opts = Opts}|_] ->
            ?DEBUG("MUC: restore room '~s'~n", [Room]),
            mod_muc_room:start(Host, ServerHost, Access,
                               Room, HistorySize,
                               RoomShaper, Opts)
    end.

-spec register_room('undefined' | ejabberd:server(), room(),
                    'undefined' | pid()) -> {'aborted',_} | {'atomic',_}.
register_room(Host, Room, Pid) ->
    F = fun() ->
                mnesia:write(#muc_online_room{name_host = {Room, Host},
                                              pid = Pid})
        end,
    mnesia:transaction(F).


-spec room_jid_to_pid(RoomJID :: ejabberd:jid()) -> {ok, pid()} | {error, not_found}.
room_jid_to_pid(#jid{luser=RoomName, lserver=MucService}) ->
    case mnesia:dirty_read(muc_online_room, {RoomName, MucService}) of
        [R] ->
        {ok, R#muc_online_room.pid};
    [] ->
        {error, not_found}
    end.

room_jid_to_pid(RoomName, MucService) ->
    case mnesia:dirty_read(muc_online_room, {RoomName, MucService}) of
        [R] ->
        {ok, R#muc_online_room.pid};
    [] ->
        {error, not_found}
    end.

-spec iq_disco_info(ejabberd:lang()) -> [jlib:xmlel(),...].
iq_disco_info(Lang) ->
    [#xmlel{name = <<"identity">>,
            attrs = [{<<"category">>, <<"conference">>},
                     {<<"type">>, <<"text">>},
                     {<<"name">>, translate:translate(Lang, <<"Chatrooms">>)}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_DISCO_INFO}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_DISCO_ITEMS}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_MUC}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_MUC_UNIQUE}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_REGISTER}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_RSM}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_VCARD}]}].


-spec iq_disco_items(ejabberd:server(), ejabberd:jid(), ejabberd:lang(),
        Rsm :: none | jlib:rsm_in()) -> any().
iq_disco_items(Host, From, Lang, none) ->
    lists:zf(fun(#muc_online_room{name_host = {Name, _Host}, pid = Pid}) ->
                     case catch gen_fsm:sync_send_all_state_event(
                                  Pid, {get_disco_item, From, Lang}, 100) of
                         {item, Desc} ->
                             flush(),
                             {true,
                              #xmlel{name = <<"item">>,
                                     attrs = [{<<"jid">>, jlib:jid_to_binary({Name, Host, <<>>})},
                                              {<<"name">>, Desc}]}};
                         _ ->
                             false
                     end
             end, get_vh_rooms(Host));
iq_disco_items(Host, From, Lang, Rsm) ->
    {Rooms, RsmO} = get_vh_rooms(Host, Rsm),
    RsmOut = jlib:rsm_encode(RsmO),
    lists:zf(fun(#muc_online_room{name_host = {Name, _Host}, pid = Pid}) ->
                     case catch gen_fsm:sync_send_all_state_event(
                                  Pid, {get_disco_item, From, Lang}, 100) of
                         {item, Desc} ->
                             flush(),
                             {true,
                              #xmlel{name = <<"item">>,
                                     attrs = [{<<"jid">>, jlib:jid_to_binary({Name, Host, <<>>})},
                                              {<<"name">>, Desc}]}};
                         _ ->
                             false
                     end
             end, Rooms) ++ RsmOut.


-spec get_vh_rooms(ejabberd:server(), jlib:rsm_in()) -> {list(), jlib:rsm_out()}.
get_vh_rooms(Host, #rsm_in{max=M, direction=Direction, id=I, index=Index}) ->
    AllRooms = lists:sort(get_vh_rooms(Host)),
    Count = erlang:length(AllRooms),
    Guard = case Direction of
                _ when Index =/= undefined ->
            [{'=:=', {element, 2, '$1'}, Host}];
                aft ->
            [{'=:=', {element, 2, '$1'}, Host},
             {'>',   {element, 1, '$1'}, I}]; %% not exact here
        before when I =/= <<>> ->
            [{'=:=', {element, 2, '$1'}, Host},
             {'<',   {element, 1, '$1'}, I}]; %% not exact here
                _ ->
            [{'=:=', {element, 2, '$1'}, Host}]
            end,
    L = lists:sort(
          mnesia:dirty_select(muc_online_room,
                              [{#muc_online_room{name_host = '$1', _ = '_'},
                                Guard,
                                ['$_']}])),
    L2 = if
             Index == undefined andalso Direction == before ->
                 lists:reverse(lists:sublist(lists:reverse(L), 1, M));
             Index == undefined ->
                 lists:sublist(L, 1, M);
             Index > Count  orelse Index < 0 ->
                 [];
             true ->
                 lists:sublist(L, Index+1, M)
         end,
    if
        L2 == [] ->
            {L2, #rsm_out{count=Count}};
        true ->
            H = hd(L2),
            NewIndex = get_room_pos(H, AllRooms),
            T=lists:last(L2),
            {F, _}=H#muc_online_room.name_host,
            {Last, _}=T#muc_online_room.name_host,
            {L2, #rsm_out{first=F, last=Last, count=Count, index=NewIndex}}
    end.

%% @doc Return the position of desired room in the list of rooms.
%% The room must exist in the list. The count starts in 0.
-spec get_room_pos(muc_online_room(), [muc_online_room()]) -> integer().
get_room_pos(Desired, Rooms) ->
    get_room_pos(Desired, Rooms, 0).
get_room_pos(Desired, [HeadRoom | _], HeadPosition)
  when (Desired#muc_online_room.name_host ==
        HeadRoom#muc_online_room.name_host) ->
    HeadPosition;
get_room_pos(Desired, [_ | Rooms], HeadPosition) ->
    get_room_pos(Desired, Rooms, HeadPosition + 1).


-spec flush() -> 'ok'.
flush() ->
    receive
        _ ->
            flush()
    after 0 ->
            ok
    end.


-spec xfield(Type :: binary(), Label :: binary(), Var :: binary(),
             Val :: binary(), ejabberd:lang()) -> jlib:xmlel().
xfield(Type, Label, Var, Val, Lang) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"type">>, Type},
                     {<<"label">>, translate:translate(Lang, Label)},
                     {<<"var">>, Var}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = Val}]}]}.


%% @doc Get a pseudo unique Room Name. The Room Name is generated as a hash of
%%      the requester JID, the local time and a random salt.
%%
%%      <<"pseudo">> because we don't verify that there is not a room
%%       with the returned Name already created, nor mark the generated Name
%%       as <<"already used">>.  But in practice, it is unique enough. See
%%       http://xmpp.org/extensions/xep-0045.html#createroom-unique
-spec iq_get_unique(ejabberd:jid()) -> jlib:xmlcdata().
iq_get_unique(From) ->
        #xmlcdata{content = sha:sha(term_to_binary([From, os:timestamp(), randoms:get_string()]))}.


-spec iq_get_register_info('undefined' | ejabberd:server(),
        ejabberd:simple_jid() | ejabberd:jid(), ejabberd:lang())
            -> [jlib:xmlel(),...].
iq_get_register_info(Host, From, Lang) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    LUS = {LUser, LServer},
    {Nick, Registered} =
        case catch mnesia:dirty_read(muc_registered, {LUS, Host}) of
            {'EXIT', _Reason} ->
                {<<>>, []};
            [] ->
                {<<>>, []};
            [#muc_registered{nick = N}] ->
                {N, [#xmlel{name = <<"registered">>}]}
        end,
    Registered ++
        [#xmlel{name = <<"instructions">>,
                children = [#xmlcdata{content = translate:translate(
                                                  Lang, <<"You need a client that supports x:data to register the nickname">>)}]},
         #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}],
                children = [#xmlel{name = <<"title">>,
                                   children = [#xmlcdata{content = translate:translate(
                                                                     Lang, <<"Nickname Registration at ">>) ++ Host}]},
                            #xmlel{name = <<"instructions">>,
                                   children = [#xmlcdata{content = translate:translate(
                                                                     Lang, <<"Enter nickname you want to register">>)}]},
                            xfield(<<"text-single">>, <<"Nickname">>, <<"nick">>, Nick, Lang)]}].


-spec iq_set_register_info(ejabberd:server(),
        ejabberd:simple_jid() | ejabberd:jid(), nick(), ejabberd:lang())
            -> {'error',jlib:xmlel()} | {'result',[]}.
iq_set_register_info(Host, From, Nick, Lang) ->
    {LUser, LServer, _} = jlib:jid_tolower(From),
    LUS = {LUser, LServer},
    F = fun() ->
                case Nick of
                    <<>> ->
                        mnesia:delete({muc_registered, {LUS, Host}}),
                        ok;
                    _ ->
                        Allow =
                            case mnesia:select(
                                   muc_registered,
                                   [{#muc_registered{us_host = '$1',
                                                     nick = Nick,
                                                     _ = '_'},
                                     [{'==', {element, 2, '$1'}, Host}],
                                     ['$_']}]) of
                                [] ->
                                    true;
                                [#muc_registered{us_host = {U, _Host}}] ->
                                    U == LUS
                            end,
                        if
                            Allow ->
                                mnesia:write(
                                  #muc_registered{us_host = {LUS, Host},
                                                  nick = Nick}),
                                ok;
                            true ->
                                false
                        end
                end
        end,
    case mnesia:transaction(F) of
        {atomic, ok} ->
            {result, []};
        {atomic, false} ->
            ErrText = <<"That nickname is registered by another person">>,
            {error, ?ERRT_CONFLICT(Lang, ErrText)};
        _ ->
            {error, ?ERR_INTERNAL_SERVER_ERROR}
    end.


-spec process_iq_register_set(ejabberd:server(), ejabberd:jid(),
        jlib:xmlel(), ejabberd:lang())
            -> {'error', jlib:xmlel()} | {'result',[]}.
process_iq_register_set(Host, From, SubEl, Lang) ->
    #xmlel{children = Els} = SubEl,
    case xml:get_subtag(SubEl, <<"remove">>) of
        false ->
            case xml:remove_cdata(Els) of
                [#xmlel{name = <<"x">>} = XEl] ->
                    case {xml:get_tag_attr_s(<<"xmlns">>, XEl),
                          xml:get_tag_attr_s(<<"type">>, XEl)} of
                        {?NS_XDATA, <<"cancel">>} ->
                            {result, []};
                        {?NS_XDATA, <<"submit">>} ->
                            XData = jlib:parse_xdata_submit(XEl),
                            case XData of
                                invalid ->
                                    {error, ?ERR_BAD_REQUEST};
                                _ ->
                                    case lists:keysearch(<<"nick">>, 1, XData) of
                                        {value, {_, [Nick]}} when Nick /= <<>> ->
                                            iq_set_register_info(Host, From, Nick, Lang);
                                        _ ->
                                            ErrText = <<"You must fill in field \"Nickname\" in the form">>,
                                            {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)}
                                    end
                            end;
                        _ ->
                            {error, ?ERR_BAD_REQUEST}
                    end;
                _ ->
                    {error, ?ERR_BAD_REQUEST}
            end;
        _ ->
            iq_set_register_info(Host, From, <<>>, Lang)
    end.


-spec iq_get_vcard(ejabberd:lang()) -> [jlib:xmlel(),...].
iq_get_vcard(Lang) ->
    [#xmlel{name = <<"FN">>,
            children = [#xmlcdata{content = <<"ejabberd/mod_muc">>}]},
     #xmlel{name = <<"URL">>, children = [#xmlcdata{content = ?EJABBERD_URI}]},
     #xmlel{name = <<"DESC">>,
            children = [#xmlcdata{content = translate:translate(Lang, <<"ejabberd MUC module">>) ++
                                    <<"\nCopyright (c) 2003-2011 ProcessOne">>}]}].


-spec broadcast_service_message(ejabberd:server(), binary() | string()) -> ok.
broadcast_service_message(Host, Msg) ->
    lists:foreach(
      fun(#muc_online_room{pid = Pid}) ->
              gen_fsm:send_all_state_event(
                Pid, {service_message, Msg})
      end, get_vh_rooms(Host)).


-spec get_vh_rooms(ejabberd:server()) -> [muc_online_room()].
get_vh_rooms(Host) ->
    mnesia:dirty_select(muc_online_room,
                        [{#muc_online_room{name_host = '$1', _ = '_'},
                          [{'==', {element, 2, '$1'}, Host}],
                          ['$_']}]).


-spec clean_table_from_bad_node(node()) -> any().
clean_table_from_bad_node(Node) ->
    F = fun() ->
                Es = mnesia:select(
                       muc_online_room,
                       [{#muc_online_room{pid = '$1', _ = '_'},
                         [{'==', {node, '$1'}, Node}],
                         ['$_']}]),
                lists:foreach(fun(E) ->
                                      mnesia:delete_object(E)
                              end, Es)
        end,
    mnesia:async_dirty(F).


-spec clean_table_from_bad_node(node(), ejabberd:server()) -> any().
clean_table_from_bad_node(Node, Host) ->
    F = fun() ->
                Es = mnesia:select(
                       muc_online_room,
                       [{#muc_online_room{pid = '$1',
                                          name_host = {'_', Host},
                                          _ = '_'},
                         [{'==', {node, '$1'}, Node}],
                         ['$_']}]),
                lists:foreach(fun(E) ->
                                      mnesia:delete_object(E)
                              end, Es)
        end,
    mnesia:async_dirty(F).


-spec update_tables(ejabberd:server()) -> any().
update_tables(Host) ->
    update_muc_room_table(Host),
    update_muc_registered_table(Host).


-spec update_muc_room_table(ejabberd:server()) -> any().
update_muc_room_table(Host) ->
    Fields = record_info(fields, muc_room),
    case mnesia:table_info(muc_room, attributes) of
        Fields ->
            ok;
        [name, opts] ->
            ?INFO_MSG("Converting muc_room table from {name, opts} format", []),
            {atomic, ok} = mnesia:create_table(
                             mod_muc_tmp_table,
                             [{disc_only_copies, [node()]},
                              {type, bag},
                              {local_content, true},
                              {record_name, muc_room},
                              {attributes, record_info(fields, muc_room)}]),
            mnesia:transform_table(muc_room, ignore, Fields),
            F1 = fun() ->
                         mnesia:write_lock_table(mod_muc_tmp_table),
                         mnesia:foldl(
                           fun(#muc_room{name_host = Name} = R, _) ->
                                   mnesia:dirty_write(
                                     mod_muc_tmp_table,
                                     R#muc_room{name_host = {Name, Host}})
                           end, ok, muc_room)
                 end,
            mnesia:transaction(F1),
            mnesia:clear_table(muc_room),
            F2 = fun() ->
                         mnesia:write_lock_table(muc_room),
                         mnesia:foldl(
                           fun(R, _) ->
                                   mnesia:dirty_write(R)
                           end, ok, mod_muc_tmp_table)
                 end,
            mnesia:transaction(F2),
            mnesia:delete_table(mod_muc_tmp_table);
        _ ->
            ?INFO_MSG("Recreating muc_room table", []),
            mnesia:transform_table(muc_room, ignore, Fields)
    end.


-spec update_muc_registered_table(ejabberd:server()) -> any().
update_muc_registered_table(Host) ->
    Fields = record_info(fields, muc_registered),
    case mnesia:table_info(muc_registered, attributes) of
        Fields ->
            ok;
        [user, nick] ->
            ?INFO_MSG("Converting muc_registered table from {user, nick} format", []),
            {atomic, ok} = mnesia:create_table(
                             mod_muc_tmp_table,
                             [{disc_only_copies, [node()]},
                              {type, bag},
                              {local_content, true},
                              {record_name, muc_registered},
                              {attributes, record_info(fields, muc_registered)}]),
            mnesia:del_table_index(muc_registered, nick),
            mnesia:transform_table(muc_registered, ignore, Fields),
            F1 = fun() ->
                         mnesia:write_lock_table(mod_muc_tmp_table),
                         mnesia:foldl(
                           fun(#muc_registered{us_host = US} = R, _) ->
                                   mnesia:dirty_write(
                                     mod_muc_tmp_table,
                                     R#muc_registered{us_host = {US, Host}})
                           end, ok, muc_registered)
                 end,
            mnesia:transaction(F1),
            mnesia:clear_table(muc_registered),
            F2 = fun() ->
                         mnesia:write_lock_table(muc_registered),
                         mnesia:foldl(
                           fun(R, _) ->
                                   mnesia:dirty_write(R)
                           end, ok, mod_muc_tmp_table)
                 end,
            mnesia:transaction(F2),
            mnesia:delete_table(mod_muc_tmp_table);
        _ ->
            ?INFO_MSG("Recreating muc_registered table", []),
            mnesia:transform_table(muc_registered, ignore, Fields)
    end.


%% Get room list per user.
%% FoldFun is a function that folds user_room record to desired output.
get_user_rooms(User, Server, FoldFun) ->
  lists:foldl(FoldFun, [], get_user_rooms(User, Server)).

get_user_rooms(User, Server) ->
  %% AB#22
  %% get default room
  mod_muc_company:get_default_room_of_user({User, Server}).
  

get_room_roster(_Acc, {User, Server}) ->
   %  FoldFun = fun(#user_room{room = Room}, Roster) ->
			% 	{GroupName, _Service} = Room,
			% 	lists:foldl(fun({MemberU, MemberS}, Acc2) ->
			% 	  	ShortJid = {MemberU, MemberS, <<>>},
			% 	 	[#roster{
			% 			 usj = {MemberU, MemberS, ShortJid},
			% 			 us = {User, Server},
			% 			 jid = ShortJid,
			% 			 name = MemberU,
			% 			 subscription = both,
			% 			 ask = none,
			% 			 groups = [GroupName],
			% 			 askmessage = [],
			% 			 xs = []							
			% 			}  | Acc2] end, Roster, get_room_members(Room))				
			% end,   
    % get_user_rooms(User, Server, FoldFun).      
    case ejabberd_config:get_local_option({host_type, Server}) of
    default ->
        case binary:split(User, [<<"_">>]) of
        [<<"bot">>, _] ->
            %% TODO : no need handle for bot 
            []; 
        [_, ORGID] ->
            UsersOfDefaultRoom = get_users_of_default_room(ORGID, Server),
            lists:foldl(
                fun({MemberU, MemberS}, Acc) when 
                                    MemberU == User andalso MemberS == Server -> 
                    Acc ;
                ({MemberU, MemberS}, Acc) ->
                    ShortJid = {MemberU, MemberS, <<>>},
                    [#roster{ 
                        usj = {MemberU, MemberS, ShortJid},
                        us = {User, Server},
                        jid = ShortJid,
                        name = MemberU,
                        subscription = both,
                        ask = none,
                        groups = [ORGID],
                        askmessage = [],
                        xs = []
                    } | Acc];
                (_, Acc) -> Acc 
            end, [], UsersOfDefaultRoom);
        _ -> [] 
        end;
    _ -> [] 
    end.



get_room_members({RoomName, RoomService} = _Room) ->
  mod_muc_company:get_room_members(RoomName, RoomService).

get_users_of_default_room(ORGID, Server) ->
    ValidateOrig =  muc_util:validate_orgid_from_web_db_xmpp_db(Server, ORGID),
    if ValidateOrig == true -> 
        MucService = muc_util:get_muc_service(Server),
        muc_util:get_room_affiliations(ORGID, MucService);
    true -> []
    end.

delete_user_rooms(User, Server) ->
    F = fun() -> mnesia:delete({user_room, {User, Server}}) end,
    mnesia:transaction(F),
    ok.

migrate_data() ->
    %% get all data of muc_room
    Keys = mnesia:dirty_all_keys(muc_room),
    Data = lists:map(fun(Key) ->
                            mnesia:dirty_read({muc_room, Key})
                      end, Keys),

    %% get info of us (user, server), room, affiliation
    lists:foreach(fun(RoomInfo) ->
        [{_, RoomName, Opts}] = RoomInfo,
            case lists:keyfind(affiliations, 1, Opts) of
                false ->
                    ok;
                {_, Affiliations} ->
                    lists:foreach(fun({{U, S, _R}, Affiliation}) ->
                                %% Store data
                                Rec = #user_room{us = {U, S}, room = RoomName, affiliation = Affiliation},
                                F = fun() -> mnesia:write(Rec) end,
                                mnesia:transaction(F)
                                end, Affiliations)
            end
        end, Data).

save_company_room(Host, Room, Default) ->
	%% get company_id and room_id
	%% save to table company_room in postgresql
	case Default of 
		false ->
            LRoom = binary_to_list(Room),
			Company_room = string:tokens(LRoom, "_"),
			case Company_room of 
               [LRoom] -> mod_company_odbc:add_company_room(Host, Room, Room, 1); 
			   [RoomId | CompanyId] when RoomId=/=[] -> 
					mod_company_odbc:add_company_room(Host, 
									list_to_binary(CompanyId), list_to_binary(RoomId), 0);
				_ ->					
					false
			end;
		true ->
			mod_company_odbc:add_company_room(Host, Room, Room, 1)
	end.

get_default_value(Packet, Room) ->
	case xml:get_subtag(Packet, <<"x">>) of
		false ->
			false;
		XTag ->
			case xml:get_subtag_cdata(XTag, <<"default">>) of
				<<"1">> -> ?INFO_MSG("Default room: ~p~n", [XTag]), true;
				_ -> 
                    case binary:split(Room, <<"_">>) of
                        [Room] -> true;
                         _ -> ?INFO_MSG("Normal room: ~p~n", [XTag]), false
                    end 
			end;
		_ ->
            ?ERROR_MSG("Normal room: Missing XTag Field ~n", []),
			false
    end.

%% store all room when remove a user from default room
store_muc_room(JID, Host, _RoomId, RoomList) ->
	lists:foreach(fun({Room}) ->
		case mnesia:dirty_read(muc_room, {Room, Host}) of
			[] -> ok;
			[#muc_room{opts = Opts}] ->
				Affiliations = proplists:get_value(affiliations, Opts),
				NewAff = lists:keydelete(
								{JID#jid.luser, JID#jid.lserver, <<>>}, 1, Affiliations),
				NewAffiliations = {affiliations, NewAff},
				?INFO_MSG("AB#22 NewAffiliations ~p~n, Room: ~p~n", [NewAffiliations, Room]),
				NewOpts = lists:keyreplace(affiliations, 1, Opts, NewAffiliations),
				store_room(Host, Room, NewOpts)
		end
	 end, RoomList).

check_room_default(Host, Room) ->
	mod_company_odbc:check_default_room(Host, Room).