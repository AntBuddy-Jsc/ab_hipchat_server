-module (mod_social_session).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_social.hrl").
-include("mod_antbuddy_stanza.hrl").

-export([start/0,
         add_session/1,
         delete_session/1,
         get_all_id/0,
         get_session/1, 
         restore_all_session/1
    ]).

-export([register_process_social_c2s/2
		, delete_process_social_c2s/1
		, get_process_social_c2s/1
		, clean_table_process_c2s/1]).

start() ->
    mnesia:create_table(social_session,
        [{attributes, record_info(fields, social_session)},
        {disc_copies, [node()]}]),
	mnesia:create_table(process_social_c2s,
		[{ram_copies, [node()]},
		 {attributes, record_info(fields, process_social_c2s)}]),
	mnesia:add_table_copy(process_social_c2s, node(), ram_copies),
    mnesia:add_table_copy(social_session, node(), disc_copies),
	clean_table_process_c2s(node()).

add_session(#social_session{} = State) ->
    mnesia:sync_dirty(fun() ->
                      mnesia:write(State)
                      end).

delete_session(Id) -> 
    mnesia:sync_dirty(fun() ->
                        mnesia:delete({social_session, Id})
                      end).

get_all_id() -> 
    case catch mnesia:dirty_all_keys(social_session) of
        Ids when is_list(Ids) -> Ids;
        _ -> []
    end.

get_session(Id) ->
    case catch mnesia:dirty_read(social_session, Id) of
        [C2S] ->
            C2S;
        _ ->
            []
    end.

restore_all_session(Opts) -> 
    WebService = proplists:get_value(web_api, Opts),
    Ids = get_all_id(),
    lists:foreach(fun(Id) -> 
            Session = get_session(Id), 
            ProcName = mod_social_util:proc_name(Session#social_session.channel_id, Session#social_session.id),
			case mod_social_session:get_process_social_c2s(ProcName) of 
                Pid when is_pid(Pid) -> 
                    ok; % social c2s process is already exists
                _ -> 
                    mod_social_c2s:instance(Session, WebService)
            end
    end, Ids).

-spec clean_table_process_c2s(node()) -> any().
clean_table_process_c2s(Node) ->
    F = fun() ->
                Es = mnesia:select(
                       process_social_c2s,
                       [{#process_social_c2s{pid = '$1', _ = '_'},
                         [{'==', {node, '$1'}, Node}],
                         ['$_']}]),
                lists:foreach(fun(E) ->
                                      mnesia:delete_object(E)
                              end, Es)
        end,
    mnesia:async_dirty(F).

register_process_social_c2s(ProcName, Pid) ->
    F = fun() ->
                mnesia:write(#process_social_c2s{proc_name = ProcName,
                                              pid = Pid})
        end,
    mnesia:transaction(F).

delete_process_social_c2s(ProcName) ->
    mnesia:sync_dirty(fun() ->
                        mnesia:delete({process_social_c2s, ProcName})
                      end).


get_process_social_c2s(ProcName) ->
    case mnesia:dirty_read(process_social_c2s, ProcName) of
        [] ->
            [];
        [R] ->
			?INFO_MSG("#78: R: ~p~n", [R]),
            R#process_social_c2s.pid
    end.

