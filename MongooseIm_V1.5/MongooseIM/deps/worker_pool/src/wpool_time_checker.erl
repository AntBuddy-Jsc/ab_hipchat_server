% This file is licensed to you under the Apache License,
% Version 2.0 (the "License"); you may not use this file
% except in compliance with the License.  You may obtain
% a copy of the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing,
% software distributed under the License is distributed on an
% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
% KIND, either express or implied.  See the License for the
% specific language governing permissions and limitations
% under the License.
%%% @hidden
-module(wpool_time_checker).
-author('elbrujohalcon@inaka.net').

-behaviour(gen_server).

-record(state, {wpool   :: wpool:name(),
                handler :: {atom(), atom()}}).
-type state() :: #state{}.

%% api
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%%%===================================================================
%%% API
%%%===================================================================
%% @private
-spec start_link(wpool:name(), atom(), {atom(), atom()}) ->
  {ok, pid()} | {error, {already_started, pid()} | term()}.
start_link(WPool, Name, Handler) ->
  gen_server:start_link({local, Name}, ?MODULE, {WPool, Handler}, []).

%%%===================================================================
%%% init, terminate, code_change, info callbacks
%%%===================================================================
%% @private
-spec init({wpool:name(), {atom(), atom()}}) -> {ok, state()}.
init({WPool, Handler}) -> {ok, #state{wpool = WPool, handler = Handler}}.

%% @private
-spec terminate(atom(), state()) -> ok.
terminate(_Reason, _State) -> ok.

%% @private
-spec code_change(string(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Cast, State) -> {noreply, State}.

-type from() :: {pid(), reference()}.
%% @private
-spec handle_call(term(), from(), state()) -> {reply, ok, state()}.
handle_call(_Call, _From, State) -> {reply, ok, State}.

%%%===================================================================
%%% real (i.e. interesting) callbacks
%%%===================================================================
%% @private
-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info({check, Pid, TaskId, Runtime}, State) ->
  case erlang:process_info(Pid, dictionary) of
    {dictionary, Values} ->
      run_task(
        TaskId, proplists:get_value(wpool_task, Values), Pid,
        State#state.wpool, State#state.handler, Runtime);
    _ -> ok
  end,
  {noreply, State};
handle_info(_Info, State) -> {noreply, State}.

run_task(TaskId, {TaskId, _, Task}, Pid, Pool, {Mod, Fun}, Runtime) ->
  catch Mod:Fun([{alert,   overrun},
                 {pool,    Pool},
                 {worker,  Pid},
                 {task,    Task},
                 {runtime, Runtime}]),
  case 2 * Runtime of
    NewOverrunTime when NewOverrunTime =< 4294967295 ->
      erlang:send_after(
        Runtime, self(), {check, Pid, TaskId, NewOverrunTime}),
      ok;
    _ -> ok
  end;
run_task(_TaskId, _Value, _Pid, _Pool, _Handler, _Runtime) -> ok.
