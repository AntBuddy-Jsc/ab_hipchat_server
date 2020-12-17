%%% -------------------------------------------------------------------
%%% Author  : bokner
%%% Description : Message archive management (XEP-0313) for 1-to-1
%%%
%%% Created : Oct 12, 2012
%%% -------------------------------------------------------------------
-module(mod_11_timeline).

-behaviour(gen_server).
-behaviour(gen_mod).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mam.hrl").
-include("mod_antbuddy_stanza.hrl").
%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, start/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% IQ handlers
-export([process_mam_iq/3]).

%% Message hook
-export([log_packet_out/1]).
%-export([filter_packet/1]).

%% For development only
-compile(export_all).

%% Internal state
-record(state, {host}).

%% Macros
-define(PROCNAME, ejabberd_mod_msg_archive).
-define(PROC(Server), gen_mod:get_module_proc(Server, ?PROCNAME)).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Host, Opts) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
  gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start(Host, Opts) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
  ChildSpec =
    {Proc,
     {?MODULE, start_link, [Host, Opts]},
     permanent,
     1000,
     worker,
     [?MODULE]},
  supervisor:start_child(ejabberd_sup, ChildSpec).

stop(Host) ->
  Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
  supervisor:terminate_child(ejabberd_sup, Proc),
  supervisor:delete_child(ejabberd_sup, Proc). 

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Host, _Opts]) ->
  %  ejabberd_redis:start_link([
  %          {worker_config, ejabberd_config:get_local_option({redis_opts, Host})}
  %          ]),
  %% Have the module to handle the MAM traffic
  gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_MAM,
                                ?MODULE, process_mam_iq, parallel),
  gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_MAM,
                                ?MODULE, process_mam_iq, parallel),
  %% The hook to store 1-to-1 messages
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, push_edited_message, 75),
  ejabberd_hooks:add(filter_packet, global, ?MODULE, log_packet_out, 75),
  {ok, #state{host = Host}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({route, #jid{luser = _U, lserver = _S} = _From, _To,  _Packet}, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, State) ->   
    ejabberd_hooks:delete(user_send_packet, State#state.host, ?MODULE, push_edited_message, 75),
    ejabberd_hooks:delete(filter_packet, global, ?MODULE, log_packet_out, 75).

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
process_mam_iq(From, _To, #iq{type = get, sub_el = Query} = IQ) ->
  {WithJid, Start, End, Limit, Direction} = mam:resolve_request_params(From, Query),
  Result = process_mam(From, WithJid, Start, End, Limit, Direction),
  result_iq(Result, IQ, fun(Msg) ->
                             send_archive_message(From, Msg, IQ#iq.id)
            end);

process_mam_iq(From, To, IQ) ->
  ?INFO_MSG("Unexpected request to MAM ~p",[{From, To, IQ}]),
  ignore.

process_mam(JidFrom, JidTo, Start, End, Limit, Direction) ->
  get_page(JidFrom, JidTo, Start, End, Limit, Direction).

result_iq(Result, IQ, SendResultsFun) ->
    case Result of
      {ok, Page} ->
        lists:foreach(SendResultsFun, Page),
        IQ#iq{type = result, sub_el = []};
      {error, _Reason} ->
        IQ#iq{type = error, sub_el = []} 
    end.

       
%% Intercept incoming/outgoing messages of type "chat" (no type defaults to "chat")
log_packet_out({From, To, #xmlel{name = <<"message">>} = Stanza} = Packet ) when not is_binary(From)->
       Service = jlib:make_jid(<<>>, From#jid.lserver, <<>>),
       %EditTag = xml:get_subtag(Stanza,<<"replace">>),
       %DeleteTag = xml:get_subtag(Stanza,<<"delete">>),
       %NoteTag = xml:get_subtag(Stanza,<<"note">>),
       ResPacket = 
       %case {EditTag,DeleteTag,NoteTag} of
       %{false,false,false} ->        
               %% TODO : Log Chat Message
               case xml:get_tag_attr_s(<<"type">>, Stanza) of
                    Type when Type == <<"chat">>; Type == <<>> ->
                        case {xml:get_subtag(Stanza, <<"body">>),
                                              xml:get_subtag(Stanza, <<"result">>), %% used for mam 
                                              xml:get_subtag(Stanza, <<"delay">>), %% used by mod_offline 
                                              is_archived(Stanza)} %% used by archieve process
                                            of 
                            {false,_,_,_} ->
                                Packet;
                            {_,false,false,false} ->
                                BodyMsg = xml:get_subtag_cdata(Stanza, <<"body">>),
                    			Timestamp = os:timestamp(),
								BTimestamp = jlib:now_to_utc_binary_milisecs(Timestamp),
                                ?DEBUG("antbuddy_dev: Log Message: ~p~n",[BTimestamp]),
                                spawn(fun() ->
										MsgId = xml:get_tag_attr_s(<<"id">>, Stanza),
										case {xml:get_subtag(Stanza, <<"file">>),
												xml:get_subtag(Stanza, <<"oembed">>),
												xml:get_subtag(Stanza, <<"expandbody">>)} of
											%% normal message and add user message
											{false, false, false} ->
												log_msg(normal, From#jid.luser, BodyMsg, To#jid.luser, Timestamp, MsgId);
											%% message with file
											{FileData, false, false} ->
												log_msg(message_file, From#jid.luser, BodyMsg, To#jid.luser, Timestamp, FileData, MsgId);
											%% message with oembed
											{false, OembedData, false} ->
												log_msg(message_oembed, From#jid.luser, BodyMsg, To#jid.luser, Timestamp, OembedData, MsgId);
											%% bot message
											{false, false, _BotData} ->
												ExpandBody = xml:get_subtag_cdata(Stanza, <<"expandbody">>),
												log_msg(message_bot, From#jid.luser, BodyMsg, To#jid.luser, Timestamp, ExpandBody, MsgId);
											_ -> 
                                                Packet
										end
                                end),
                                NewStanza = create_new_pack(Stanza, BTimestamp),
                                %%Send echo message to sender
								BareJID = From#jid{resource = <<>>, lresource = <<>>},
                                ejabberd_router:route(Service, BareJID, NewStanza),
                                ejabberd_hooks:run(push_notification, To#jid.lserver, [From, To, NewStanza]),
                                {From, To, NewStanza};
                           _ -> Packet
                         end;
                _Other ->
                    %void
                    Packet
            end,
        % _ ->
        %       ?INFO_MSG("kabam_dev: No Log Edit/Delete/Note Event Message ~n",[]),
        %        %void
        %        ejabberd_hooks:run(push_notification, To#jid.lserver, [From, To, Stanza]),
        %        Packet
        %end,
       ResPacket ;
 
log_packet_out(Packet) ->
        Packet.
    %void.

push_edited_message(From, To, #xmlel{name = <<"message">>} = Stanza ) -> 
    EditTag = xml:get_subtag(Stanza, <<"replace">>),
    Ts = xml:get_tag_attr_s(<<"timestamp">>, Stanza),
    Type = xml:get_tag_attr_s(<<"type">>, Stanza),
    if 
      EditTag /=false andalso Ts /= <<>> andalso Type == <<"chat">> -> 
              ?DEBUG("#67: Stanza=~p ~n", [Stanza]),
              Service = jlib:make_jid(<<>>, From#jid.lserver, <<>>),
              BareJID = From#jid{resource = <<>>, lresource = <<>>},
              ejabberd_router:route(Service, BareJID, Stanza),
              ejabberd_hooks:run(push_notification, To#jid.lserver, [From, To, Stanza]);
      true -> do_no_thing
    end;
push_edited_message(_, _, _) -> do_no_thing.

stb(String) ->
        case String of
          Val when is_list(Val) ->
              erlang:list_to_binary(Val);
          Val when is_integer(Val) ->
                 integer_to_binary(Val);
          _ ->
              String
        end.

is_archived(Packet) ->
  case catch xml:get_tag_attr_s(<<"timestamp">>, Packet) of
       <<>> ->
            false;
       _ ->
            true
  end.


create_new_pack(#xmlel{attrs = Attrs} = Packet, BTimestamp) ->
  NewAttrs = lists:append(Attrs, [{<<"timestamp">>, BTimestamp}]),
  Packet#xmlel{attrs = NewAttrs}.

%% store with bot message
log_msg(message_bot, From, BodyMsg, To, Ts, ExpandBody, Message_id) ->
	Doc = [{body, BodyMsg}, {?FROM_KEY, From}, {id, Message_id}, {?RECEIVER_KEY, To}, {?SENDER_KEY, From}, {expandBody, ExpandBody}, {isModified, <<"false">>}, {subtype, <<"bot-gitlab">>}, {type, <<"chat">>},{time, Ts}, {<<"__v">>, 0} ],
    mod_mongo:insert(Doc);

%% store message with oembed
log_msg(message_oembed, From, BodyMsg, To, Ts, OembedData, Message_id) ->
	OembedInfo = [{html,				xml:get_subtag_cdata(OembedData, ?OEMBED_HTML)}, 
					{author_url,		xml:get_subtag_cdata(OembedData, ?AUTHOR_URL)}, 
					{author_name,		xml:get_subtag_cdata(OembedData, ?AUTHOR_NAME)}, 
					{thumbnail_height,	xml:get_subtag_cdata(OembedData, ?OEMBED_THUMBNAIL_HEIGHT)}, 
					{type,				xml:get_subtag_cdata(OembedData, ?OEMBED_TYPE)}, 
					{provider_name,		xml:get_subtag_cdata(OembedData, ?PROVIDER_NAME)}, 
					{version,			xml:get_subtag_cdata(OembedData, ?OEMBED_VERSION)}, 
					{thumbnail_url,		xml:get_subtag_cdata(OembedData, ?OEMBED_THUMBNAIL_URL)}, 
					{url,				xml:get_subtag_cdata(OembedData, ?OEMBED_URL)}, 
					{thumbnail_width,	xml:get_subtag_cdata(OembedData, ?OEMBED_THUMBNAIL_WIDTH)}, 
					{mean_alpha,		xml:get_subtag_cdata(OembedData, ?OEMBED_MEAN_ALPHA)}, 
					{title,				xml:get_subtag_cdata(OembedData, ?TITLE)}, 
					{description,		xml:get_subtag_cdata(OembedData, ?DESCRIPTION)}, 
					{provider_url,		xml:get_subtag_cdata(OembedData, ?PROVIDER_URL)}],
	Doc = [{?SENDER_KEY, From}, {body, BodyMsg}, {oembed, OembedInfo }, {?FROM_KEY, From}, {?RECEIVER_KEY, To}, {id, Message_id}, {type, <<"chat">>}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
	mod_mongo:insert(Doc);

%% store message with file
log_msg(message_file, From, BodyMsg, To, Ts, FileData, Message_id) ->
	FileInfo = [{thumbnailUrl,			xml:get_subtag_cdata(FileData, ?FILE_THUMBNAIL_URL)}, 
				{thumbnailWidth,		xml:get_subtag_cdata(FileData, ?FILE_THUMBNAIL_WIDTH)}, 
				{thumbnailHeight,		xml:get_subtag_cdata(FileData, ?FILE_THUMBNAIL_HEIGHT)}, 
				{mimeType,				xml:get_subtag_cdata(FileData, ?MIMETYPE)}, 
				{fileUrl,				xml:get_subtag_cdata(FileData, ?FILE_URL)}, 
				{size,					xml:get_subtag_cdata(FileData, ?FILE_SIZE)}, 
				{name,					xml:get_subtag_cdata(FileData, ?FILE_NAME)}],
	Doc = [{?SENDER_KEY, From}, {body, BodyMsg}, {file, FileInfo}, {?FROM_KEY, From}, {?RECEIVER_KEY, To}, {id, Message_id}, {type, <<"chat">>}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
	mod_mongo:insert(Doc).

%% store normal message
log_msg(normal, From, BodyMsg, To, Ts, Message_id) ->
	Doc = [{?SENDER_KEY, From}, {body, BodyMsg}, {?FROM_KEY, From}, {?RECEIVER_KEY, To}, {id, Message_id}, {type, <<"chat">>}, {time, Ts}, {<<"__v">>, 0}, {isModified, <<"false">>}],
	mod_mongo:insert(Doc).

get_page(JIDFrom, JIDTo, StartTs, EndTs, PageSize, Direction) ->
    {Selector, Optional} = page_cmd(JIDFrom, JIDTo, StartTs, EndTs, PageSize, Direction),
    case mod_mongo:fetch_last_n(Selector, Optional) of           
        {error, Reason} ->
            error_logger:info_msg("Error in get_page", [Reason]),
            {error, Reason};
      Results ->      
        Res = case Direction of 
                forward ->
                    Results;
                backward ->
                    lists:reverse(Results)
        end,
        {ok, lists:map(fun(R) -> 
        		mod_antbuddy_stanza:convert_value_to_message(JIDFrom, JIDTo, R) 
                end, Res
        )}
  end.

%% Create the output for the MAM message, and route it to the requesting Jid
send_archive_message(To, Packet, QueryID) ->
  	MAMPacket = 
    	#xmlel{
        name = <<"message">>,
        attrs = [],
        children =  
          [   
          #xmlel{
        name = <<"result">>,
        attrs = [{<<"queryid">>, QueryID},
                 {<<"xmlns">>, ?NS_MAM},
                 {<<"id">>, randoms:get_string()}],
        children = [mam:forwarded(Packet)]}
         ]},
  
  ejabberd_router:route(jlib:make_jid(<<>>, To#jid.lserver, <<>>), 
                        To, xml:replace_tag_attr(<<"type">>, <<"headline">>, MAMPacket)).

page_cmd(FromJID, ToJID, StartTs, EndTs, PageSize, Direction) ->
    StartTsBin = mam:mamts_to_timestamp(StartTs),
    EndTsBin = mam:mamts_to_timestamp(EndTs),
    Selector = [{?FROM_KEY, FromJID#jid.luser}, 
                {?RECEIVER_KEY, ToJID#jid.luser},
                {time, [{gte, StartTsBin}, 
                        {lte, EndTsBin}]
                }],
    OrderBy = case Direction of
    	forward ->  asc;
    	backward -> desc
    end,
    Optional = [{limit, PageSize}, {orderby, [{time, OrderBy}]}],
    {Selector, Optional}.


sorted_str(JidStr1, JidStr2) when JidStr2 > JidStr1 ->
  << JidStr1/binary, JidStr2/binary >>;
sorted_str(JidStr1, JidStr2) ->
  << JidStr2/binary, JidStr1/binary >>.

get_tag(BTags) ->
    case BTags of 
    <<>> ->
        [];
    _ ->
        StrTags = binary_to_list(BTags),
        TagsList = string:tokens(StrTags, ";"),
        lists:map(fun(Tag) ->
               stb(Tag)
              end, TagsList)
    end.

get_extra_tags(TagList, Field) ->
    lists:map(fun(ExtTag) ->
        %%[{<<"tag">>, ExtTag}, {<<"field">>, Field}]
        [ExtTag, Field]
    end, TagList).
