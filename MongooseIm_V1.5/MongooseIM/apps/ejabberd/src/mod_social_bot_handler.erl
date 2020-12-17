-module(mod_social_bot_handler).

-include("mod_social.hrl").

-export([init/3, terminate/3]).
-export([rest_init/2]).
-export([known_methods/2
        , allowed_methods/2
        , content_types_accepted/2
        , content_types_provided/2
        , resource_exists/2
        , delete_completed/2
        , delete_resource/2
        , is_authorized/2]).
-export([to_json/2]).
-export([handle_from_json/2]).
%%% function to access bot admin account
-export([add_bot_admin/2
        , delete_bot_admin/1
        , get_bot_admin/1]).
-export([get_list_of_bot/2]).
-export([check_auth_token/1]).

-record(bot_state, {}).

init({tcp, http}, _Req, _) ->
    {upgrade, protocol, cowboy_rest};
init(_Type, _Req, _) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req0, [_Host, _Opts]) ->
    {ok, Req0, #bot_state{}}.

rest_terminate(_Req, _State) ->
    ok.

terminate(_Reason, _Req, _State) ->
    ok.

%%%======================================
%%% Cowboy HTTP Rest API Callbacks
%%% GET     /bots
%%% GET     /bots/id
%%% POTS    /bots
%%% PUT     /bots/id
%%% DELETE  /bots/id
%%%======================================
known_methods(Req, State) ->
    ?DEBUG("run: known_methods ~n", []),
    {?ALLOWED_METHODS, Req, State}.

allowed_methods(Req0, State) ->
    ?DEBUG("run: allowed_methods~n", []),
    AllowedMethods = case cowboy_req:bindings(Req0) of
        {[], _} ->
            [?HTTP_GET, ?HTTP_POST];
        {[{bot_id, _}], _} ->
                [?HTTP_GET, ?HTTP_POST, ?HTTP_PUT, ?HTTP_DELETE];
        _ ->
            []
    end,
    {AllowedMethods, Req0, State}.

%% Methods: POST, PUT, PATCH
content_types_accepted(Req, State) ->
    ?DEBUG("run: content_types_accepted Req: ~p~n", [Req]),
    {[{{<<"application">>, <<"json">>, []}, handle_from_json}],
        Req, State}.

%% Methods: GET, HEAD
content_types_provided(Req, State) ->
    ?DEBUG("run: content_types_provided Req: ~p~n", [Req]),
    {[
        {{<<"application">>, <<"json">>, '*'}, to_json}
    ], Req, State}.

%% Methods: DELETE
delete_resource(Req, State) ->
    ?DEBUG("run: delete_resource~n", []),
    {Binding, Req2} = cowboy_req:binding(bot_id, Req),
    Res = delete_bot(Req2, State, Binding),
    {Res, Req, State}.

%% Methods: DELETE
delete_completed(Req, State) ->
    ?DEBUG("run: delete_completed~n", []),
    {true, Req, State}.

resource_exists(Req, State) ->
    ?DEBUG("run: resource_exists Req: ~p~n", [Req]),
    {AllBindings, _} = cowboy_req:bindings(Req),
    IsExist = case AllBindings of
        [] ->
            true;
        [{bot_id, _}] ->
            true;
        _ ->
            false
    end,
    {IsExist, Req, State}.

%%% echo -n "Aladdin:OpenSesame" | base64
is_authorized(Req, State) ->
    ?DEBUG("run: is_authorized ~n", []),
    {Binding, _} = cowboy_req:binding(bot_id, Req),
    if Binding == <<"auth">> ->
        {true, Req, State};
      true ->
        case cowboy_req:parse_header(<<"authorization">>, Req) of
            {ok, {Token, _}, _} ->
                case check_auth_token(Token) of
                    true ->
                        delete_auth_token(Token),
                        {true, Req, State};
                    _ ->
                        {{false, <<"Wrong token">>}, Req, State}
                end;
            _ ->
                {{false, <<"Basic realm=\"cowboy\"">>}, Req, State}
        end
    end.
     

to_json(Req, State) ->
    ?DEBUG("to_json~n", []),
    case handle_request(Req, State) of
        [] ->
            {ok, Req2} = cowboy_req:reply(404, [{<<"content-type">>, <<"application/json">>}], 
                                            [], Req),
            {halt, Req2, State};
        Body ->
            ?INFO_MSG("Body: ~p~n", [Body]),
            {jsx:encode(Body), Req, State}
    end.

% {"bot_app_id": "Antbuddy", 
% "ms_app_id": "hdjksahdksahdjsajkdsajkd", 
% "ms_app_secret": "nsadsadksa", 
% "fb_token": "nsadsadksa", 
% "kite_app_id": "dsadksaldka",			// kite_app_id_for_facebook
% "kite_app_id_for_skype": "dsadksaldka",}
handle_from_json(Req, State) ->
    ?DEBUG("handle_from_json: Req: ~p~n", [Req]),
    {Binding, _} = cowboy_req:binding(bot_id, Req),
    {ok, PostVals, Req2} = cowboy_req:body(Req),
    {Method, Req3} = cowboy_req:method(Req2),
    ?INFO_MSG("handle_from_json: Binding: ~p~n", [Binding]),
    if Binding == <<"auth">> ->
        %% TODO
        handle_bot_auth(Method, PostVals, Req3, State);
      true ->
        handle_bot_info(Method, PostVals, Req3, State)
    end.

handle_request(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {AllBindings, Req3} = cowboy_req:bindings(Req2),
    case AllBindings of
        [] ->
            validate_method(Method, Req3, State);
        [{bot_id, BotId}] ->
            validate_method(Method, Req3, State, BotId);
        _ ->
            [{<<"result">>, <<"Wrong request">>}]
    end.

validate_method(?HTTP_GET, Req, State) ->
    get_list_of_bot(Req, State);
validate_method(_, _, _) ->
    [].

validate_method(?HTTP_GET, Req, State, BotId) ->
    get_detail_bot(Req, State, BotId);
validate_method(_, _, _, _) ->
    [].

handle_bot_info(?HTTP_POST, BodyVals, Req, State) ->
    case BodyVals of
        [{BodyReq, true}] ->
            create_new_bot(jsx:decode(BodyReq), Req, State);
        [{true, BodyReq}] ->
            create_new_bot(jsx:decode(BodyReq), Req, State);
        BodyReq ->
            create_new_bot(jsx:decode(BodyReq), Req, State);
        _ ->            
            {true, Req, State}
    end;
handle_bot_info(?HTTP_PUT, BodyVals, Req, State) ->
    {Binding, Req2} = cowboy_req:binding(bot_id, Req),
    ?INFO_MSG("Binding: ~p~n", [Binding]),
    case BodyVals of
        [{BodyReq, true}] ->
            update_bot(Binding, jsx:decode(BodyReq), Req2, State);
        [{true, BodyReq}] ->
            update_bot(Binding, jsx:decode(BodyReq), Req2, State);
        BodyReq ->
            update_bot(Binding, jsx:decode(BodyReq), Req2, State);
        _ ->
            {true, Req2, State}
    end;
handle_bot_info(_, _, Req, State) ->
    {true, Req, State}.

handle_bot_auth(?HTTP_POST, BodyVals, Req, State) ->
    ?DEBUG("handle_bot_auth: ~p~n", [BodyVals]),
    case BodyVals of
        [{BodyReq, true}] ->
            create_auth(jsx:decode(BodyReq), Req, State);
        [{true, BodyReq}] ->
            create_auth(jsx:decode(BodyReq), Req, State);
        BodyReq ->
            create_auth(jsx:decode(BodyReq), Req, State);
        _ ->
            {true, Req, State}
    end;
handle_bot_auth(_, _, Req, State) ->
    {true, Req, State}.

replace_modified_time(Record) ->
	case proplists:get_value(<<"modify_at">>, Record) of
		undefined ->
			Record;
		ModifiedTime when is_tuple(ModifiedTime) ->
			lists:keyreplace(<<"modify_at">>, 1, Record,
					{<<"modify_at">>, calendar:now_to_universal_time(ModifiedTime)});
        _ ->
            Record
	end.

get_list_of_bot(_Req, _State) ->
	Records = mod_mongo:fetch_all_bots(),
    Result = lists:foldl(fun(Record, Acc) ->
            case lists:keyfind(<<"bot_account">>, 1, Record) of
                false ->
			         New = lists:keydelete(<<"_id">>, 1, Record),
			         Acc ++ [replace_modified_time(New)];
                _ ->
                     Acc
            end
	    end, [], Records),
    [{<<"result">>, Result}].

create_auth(BodyReq, Req, State) ->
    User = proplists:get_value(<<"user">>, BodyReq),
    Server = proplists:get_value(<<"server">>, BodyReq),
    Password = proplists:get_value(<<"password">>, BodyReq),
    case ejabberd_auth_showdme:check_password(User, Server, Password) of
        true ->
            ?INFO_MSG("create_auth: ~n", []),
            Email = << User/binary, "@", Server/binary >>,
            Token = mod_social_util:generate_token(User, Password),
            add_bot_admin(Email, Token),
            {ok, Req2} = cowboy_req:reply(201, [{<<"content-type">>, <<"application/json">>}], 
                            jsx:encode([{<<"result">>, [{<<"auth_token">>, Token}]}]), Req),
            {halt, Req2, State};
          _ ->
            {true, Req, State}
    end.

create_new_bot(BodyReq, Req, State) ->
	NewBodyReq = BodyReq ++ [{<<"create_at">>, os:timestamp()}, {<<"modify_at">>, os:timestamp()}],
    ?DEBUG("BodyReq: ~p~n", [BodyReq]),
    Chk = check_existing_bot(BodyReq),
    case Chk of
        false ->
            case mod_mongo:create_social_bot(BodyReq) of
                ok ->
                    BotId = proplists:get_value(<<"bot_app_id">>, NewBodyReq),
                    %%{true, <<$/, "v1/bots/", BotId/binary>>};
                    {ok, Req2} = cowboy_req:reply(201, [{<<"content-type">>, <<"application/json">>}], 
                            jsx:encode([{<<"result">>, BodyReq ++ [{<<"Location">>, BotId}]}]), Req),
                    {halt, Req2, State};
                _ ->
                    {true, Req, State}
            end;
        _ -> 
            {true, Req, State}
    end.    

check_existing_bot(BodyReq) ->
    case lists:keyfind(<<"ms_app_id">>, 1, BodyReq) of
        false -> false;
        MS_App_Id -> 
            case mod_mongo:fetch_social_bot([MS_App_Id]) of
                [] -> false;
                _ -> true
            end
    end.

get_detail_bot(_Req, _State, BotId) ->
	Selector = [{<<"bot_app_id">>, BotId}],
    Res = case mod_mongo:fetch_social_bot(Selector) of
        [] ->
            [];
        BotInfo ->
            Record = lists:last(BotInfo),
			replace_modified_time(Record)
    end,
    lists:keydelete(<<"_id">>, 1, Res).

update_bot(Binding, Body, Req, State) ->
    case check_existing_bot(Body) of
        true ->
        	NewBody = 
            case proplists:get_value(<<"modify_at">>, Body) of
                undefined -> 
                    Body ++ [{<<"modify_at">>, os:timestamp()}];
                _ -> 
                    Body
            end,
            case mod_mongo:update_social_bot([{<<"ms_app_id">>, Binding}],
                                                                    NewBody) of
                ok ->
                    BotAppId = proplists:get_value(<<"bot_app_id">>, NewBody),
                    ets:insert(track_social_bot, {BotAppId, NewBody}),
                    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], 
                                            jsx:encode([{<<"result">>, Body}]), Req),
                    {halt, Req2, State};
                _ ->
                    {true, Req, State}
            end;
        _ ->
            {ok, Req3} = cowboy_req:reply(404, [{<<"content-type">>, <<"application/json">>}], 
                                            jsx:encode([{<<"result">>, <<"bot not existing">>}]), Req),
            {halt, Req3, State}
    end.

delete_bot(Req, State, BotId) ->
    Selector = [{<<"bot_app_id">>, BotId}],
    case mod_mongo:fetch_social_bot(Selector) of
        [] ->
            {ok, Req2} = cowboy_req:reply(404, 
                                    [{<<"content-type">>, <<"application/json">>}], 
                                    [], Req),
            {halt, Req2, State};
        _ ->
            case mod_mongo:delete_social_bot(Selector) of
                ok ->
                    {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], 
                                    jsx:encode([{<<"result">>, <<"Deleted ", BotId/binary>>}]), Req),
                    {halt, Req2, State};
                _ ->
                    false
            end
    end.


%%%======================================================
%%% CRUD of account that can access to manage MS Bot
%%%======================================================
add_bot_admin(Email, Password) ->
    State = #bot_admin{email = Email, password = Password},
    mnesia:sync_dirty(fun() ->
                      mnesia:write(State)
                      end).

delete_bot_admin(Email) -> 
    mnesia:sync_dirty(fun() ->
                        mnesia:delete({bot_admin, Email})
                      end).

get_bot_admin(Email) ->
    case catch mnesia:dirty_read(bot_admin, Email) of
        [#bot_admin{email = Email, password = Password}] ->
            Password;
        _ ->
            []
    end.

check_auth_token(Token) ->
    Patt = #bot_admin{email = '_', password = Token},
    case mnesia:dirty_match_object(bot_admin, Patt) of
        [] -> false;
         _ -> true
    end.

delete_auth_token(Token) ->
    Patt = #bot_admin{email = '_', password = Token},
    case mnesia:dirty_match_object(bot_admin, Patt) of
        [] -> ok;
        [Object] -> 
            mnesia:dirty_delete_object(bot_admin, Object)
    end.
