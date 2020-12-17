-module(mod_debug_account).
-behaviour(gen_mod).
-behaviour(gen_server).

-export([start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ejabberd.hrl").
-export([get_user_resources/2
		, get_session_ip/3
		, get_session/3
		, get_sessions_of_server/1
		, get_full_session_list/0]).
-record(state, {session}).

start(Host, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host,Opts],[]),
    ok.

stop(Host) ->
    ok.

init([Host,Opts]) ->
    {ok, #state{session = []}}.

handle_call(reset, _From, State) ->
    {reply, ok, State#state{session = []}};

handle_call(stop,_From, State) ->
   {stop, normal, ok, State};

handle_call(_ ,_From, State) ->
   {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(Msg, State) ->
    {noreply, State}.

get_user_resources(User, Server) ->
	ejabberd_sm:get_user_resources(User, Server).

get_session_ip(User, Server, Resource) ->
	ejabberd_sm:get_session_ip(User, Server, Resource).

get_session(User, Server, Resource) ->
	ejabberd_sm:get_session(User, Server, Resource).

get_sessions_of_server(Server) ->
	ejabberd_sm:get_vh_session_list(Server).

get_full_session_list() ->
	ejabberd_sm:get_full_session_list().
