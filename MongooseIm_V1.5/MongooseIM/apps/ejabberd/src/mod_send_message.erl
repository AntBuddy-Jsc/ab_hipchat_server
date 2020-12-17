-module(mod_send_message).
-behaviour(gen_mod).
-behaviour(gen_server).

-export([start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("ejabberd.hrl").

-record(state, {count_message}).

start(Host, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host,Opts],[]),
    ok.

stop(Host) ->
    ok.

init([Host,Opts]) ->
    {ok, #state{count_message = 0}}.

handle_call(reset, _From, State) ->
    {reply, ok, State#state{count_message = 0}};

handle_call(get, _From, State) ->
    {reply, State#state.count_message, State};

handle_call(count, _From, #state{count_message = Count} = State) ->
    {noreply, State#state{count_message = Count + 1}};

handle_call(stop,_From, State) ->
   {stop, normal, ok, State}.



handle_cast(count, #state{count_message = Count} = State) ->
    %%?INFO_MSG("Counting message ~p~n", [NewCount]),
    {noreply, State#state{count_message = Count + 1}};

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(count, #state{count_message = Count} = State) ->
    {noreply, State#state{count_message = Count + 1}};
handle_info(Msg, State) ->
	{noreply, State}.

