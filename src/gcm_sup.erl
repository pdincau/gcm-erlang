-module(gcm_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/3]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Name, ApiKey, ErrorFun) ->
    supervisor:start_child(?MODULE, [Name, ApiKey, ErrorFun]).

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(gcm, worker)]} }.

