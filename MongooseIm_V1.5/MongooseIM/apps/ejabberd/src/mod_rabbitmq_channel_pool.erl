-module(mod_rabbitmq_channel_pool).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 1000, 3600}, []}}.

pool_channel_name() ->
	?MODULE.

