-module(mod_debug_muc).
-behaviour(gen_mod).
-behaviour(gen_server).

-export([start/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_affiliations_of_process/2
		, get_affiliations_of_muc_room/2
		, get_members_online_room/2]).

-include("ejabberd.hrl").

-record(state, {affiliations}).

start(Host, Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Host,Opts],[]),
    ok.

stop(Host) ->
    ok.

init([Host,Opts]) ->
    {ok, #state{affiliations = []}}.

handle_call(reset, _From, State) ->
    {reply, ok, State#state{affiliations = []}};

handle_call({get_affiliations, Name, Host}, _From, State) ->
	Affiliations = case get_pid_room(Name, Host) of
		nonexistence -> [];
		Pid -> 
			case catch gen_fsm:sync_send_all_state_event(
						Pid, get_affiliations, 100) of
				<<>> ->
					[];
				Res ->
					Res
			end
	end,
    {reply, Affiliations, State#state{affiliations = Affiliations}};

handle_call({get_member_online, Name, Host}, _From, State) ->
    Affiliations = case get_pid_room(Name, Host) of
        nonexistence -> [];
        Pid ->
            case catch gen_fsm:sync_send_all_state_event(
                        Pid, get_member_online, 100) of
                <<>> ->
                    [];
                Res ->
                    Res
            end
    end,
    {reply, Affiliations, State#state{affiliations = Affiliations}};

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

%% Get all affiliations in process
get_affiliations_of_process(Name, Host) ->
	Pid = get_pid(),
	gen_server:call(Pid, {get_affiliations, Name, Host}).

%% Get all affliations depends on database mnesia
get_affiliations_of_muc_room(Name, Host) ->
	case mnesia:dirty_read(muc_room, {Name, Host}) of 
		[{_, _, Opts}] ->  
			proplists:get_value(affiliations, Opts);
		_ ->
			nothing
	end.

%% Get all current members in room
get_members_online_room(Name, Host) ->
    Pid = get_pid(),
    gen_server:call(Pid, {get_member_online, Name, Host}).

%%=======================================
%%	INTERNAL FUNCTIONS
%%=======================================
get_pid_room(Name, Host) ->
	case mnesia:dirty_read(muc_online_room, {Name, Host}) of
		[{muc_online_room, {_, _}, Pid}] -> Pid;
		_ -> nonexistence
	end.

get_pid() ->
	erlang:whereis(mod_debug_muc).
