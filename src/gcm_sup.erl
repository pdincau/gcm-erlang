-module(gcm_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/2]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(atom(),string()) ->
       {'error',_} | {'ok','undefined' | pid()} | {'ok','undefined' | pid(),_}.
start_child(Name, ApiKey) ->
    Pool = get_pool(),
    PoolName = proplists:get_value(pool_name, Pool, mypool),
    start_hackney(PoolName, hackney_options(Pool)),
    supervisor:start_child(?MODULE, [Name, ApiKey]).

-spec init([]) -> {ok, {{supervisor:strategy(), 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{simple_one_for_one, 5, 10}, [?CHILD(gcm, worker)]}}.


get_pool() ->
    {ok, Config} = application:get_env(gcm, hackney).

start_hackney(PoolName, Options) ->
    hackney_pool:start_pool(PoolName, Options).

hackney_options(Pool) ->
    [{timeout,         proplists:get_value(timeout, Pool, 150000)},
     {max_connections, proplists:get_value(max_connections, Pool, 100)}].