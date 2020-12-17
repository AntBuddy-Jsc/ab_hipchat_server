-module(mod_social_service).

-bihavior(gen_server).

-include("ejabberd.hrl").
-include("mod_social.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_link/0, start_child/2, delete_child/2]).

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_child(SupRef, ChildSpec) -> 
    gen_server:call(?MODULE, {create, SupRef, ChildSpec}).

delete_child(SupRef, Id) -> 
    gen_server:cast(?MODULE, {delete, SupRef, Id}).

init([]) -> 
    mnesia:create_table(bot_admin,
        [{attributes, record_info(fields, bot_admin)},
        {disc_copies, [node()]}]),
    mnesia:add_table_copy(bot_admin, node(), disc_copies),
	net_kernel:monitor_nodes(true),
    {ok, []}.

handle_info({nodedown, Node}, State) ->
	?INFO_MSG("#78: Node down: ~p~n", [Node]),
    mod_social_session:clean_table_process_c2s(Node),
    {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> 
    ok.

handle_call({create, SupRef, ChildSpec}, _From, State) -> 
    Res = case supervisor:start_child(SupRef, ChildSpec) of 
                {ok, Child} -> 
                    {ok, Child};
                {ok, Child, _Info} -> 
                    {ok, Child};
                {error, {already_started, Child}} -> 
                    {ok, Child};
                {error, Msg} -> 
                    {error, Msg};
                _ -> 
                    {error, "Cannot start child"}
            end,
    case Res of 
        {ok, Proc} when is_pid(Proc) -> 
            {reply, {ok, Proc}, State};
        {error, Message} -> 
            {reply, {error, Message}, State}
    end;
handle_call(_Request, _From, State) -> 
    {reply, {error, bad_request}, State}.


handle_cast({delete, SupRef, Id}, State) -> 
    supervisor:terminate_child(SupRef, Id),
    case supervisor:delete_child(SupRef, Id) of
        ok ->
            ?INFO_MSG("Delete Child Id: ~p from supervisor: ~p OK~n", [Id, SupRef]);
        {error, Error} -> 
            ?ERROR_MSG("Cannot delete Child Id: ~p from supervisor: ~p, error: ~p~n", [Id, SupRef, Error]);
        _ -> 
            ?ERROR_MSG("Cannot delete Child Id: ~p from supervisor: ~p~n", [Id, SupRef])
    end,
    {noreply, State};
handle_cast(_Request, State) -> 
    {noreply, State}.
    

code_change(_OldVsn, State, _Extra) -> {ok, State}.
