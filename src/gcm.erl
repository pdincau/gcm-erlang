-module(gcm).
-behaviour(gen_server).

-export([start/2, start/3, stop/1, start_link/2, start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([push/3, sync_push/3, update_error_fun/2]).

-define(SERVER, ?MODULE).

-record(state, {key, retry_after, error_fun}).

start(Name, Key) ->
    start(Name, Key, fun handle_error/2).

start(Name, Key, ErrorFun) ->
    gcm_sup:start_child(Name, Key, ErrorFun).

stop(Name) ->
    gen_server:call(Name, stop).

push(Name, RegIds, Message) ->
    gen_server:cast(Name, {send, RegIds, Message}).

sync_push(Name, RegIds, Message) ->
    gen_server:call(Name, {send, RegIds, Message}).

update_error_fun(Name, Fun) ->
    gen_server:cast(Name, {error_fun, Fun}).

%% OTP
start_link(Name, Key) ->
    start_link(Name, Key, fun handle_error/2).

start_link(Name, Key, ErrorFun) ->
    gen_server:start_link({local, Name}, ?MODULE, [Key, ErrorFun], []).

init([Key, ErrorFun]) ->
    {ok, #state{key=Key, retry_after=0, error_fun=ErrorFun}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({send, RegIds, Message}, _From, #state{key=Key} = State) ->
    Reply = do_push(RegIds, Message, Key, undefined),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({send, RegIds, Message}, #state{key=Key, error_fun=ErrorFun} = State) ->
    do_push(RegIds, Message, Key, ErrorFun),
    {noreply, State};

handle_cast({error_fun, Fun}, State) ->
    NewState = State#state{error_fun=Fun},
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal
do_push(RegIds, Message, Key, ErrorFun) ->
    error_logger:info_msg("Message=~p; RegIds=~p~n", [Message, RegIds]),
    case gcm_api:push(RegIds, Message, Key) of
        {ok, GCMResult} ->
            handle_result(GCMResult, RegIds, ErrorFun);
        {error, {retry, RetryTime}} ->
            do_backoff(RetryTime, RegIds, Message, Key, ErrorFun),
            {error, retry};
        {error, Reason} ->
            {error, Reason}
    end.

handle_result(GCMResult, RegIds, undefined) ->
    {_MulticastId, _SuccessesNumber, _FailuresNumber, _CanonicalIdsNumber, Results} = GCMResult,
    lists:map(fun({Result, RegId}) -> 
		      parse_result(Result, RegId, fun(E, I) -> {E, I} end) 
	      end, lists:zip(Results, RegIds));

handle_result(GCMResult, RegIds, ErrorFun) ->
    {_MulticastId, _SuccessesNumber, FailuresNumber, CanonicalIdsNumber, Results} = GCMResult,
    case to_be_parsed(FailuresNumber, CanonicalIdsNumber) of
        true ->
            lists:foreach(fun({Result, RegId}) -> parse_result(Result, RegId, ErrorFun) end,
			  lists:zip(Results, RegIds));
        false ->
            ok
    end.

do_backoff(RetryTime, RegIds, Message, Key, ErrorFun) ->
    case RetryTime of
	{ok, Time} ->
	    timer:apply_after(Time * 1000, ?MODULE, do_push, [RegIds, Message, Key, ErrorFun]);
	no_retry ->
	    ok
    end.

to_be_parsed(0, 0) -> false;

to_be_parsed(_FailureNumber, _CanonicalNumber) -> true.

parse_result(Result, RegId, ErrorFun) ->
    case {
      proplists:get_value(<<"error">>, Result),
      proplists:get_value(<<"message_id">>, Result),
      proplists:get_value(<<"registration_id">>, Result)
     } of
        {Error, undefined, undefined} when Error =/= undefined ->
            ErrorFun(Error, RegId);
        {undefined, MessageId, undefined} when MessageId =/= undefined ->
            ok;
        {undefined, MessageId, NewRegId} when MessageId =/= undefined andalso NewRegId =/= undefined ->
            ErrorFun(<<"NewRegistrationId">>, {RegId, NewRegId})
    end.

handle_error(<<"NewRegistrationId">>, {RegId, NewRegId}) ->
    error_logger:info_msg("Message sent. Update id ~p with new id ~p.~n", [RegId, NewRegId]),
    ok;

handle_error(<<"Unavailable">>, RegId) ->
    %% The server couldn't process the request in time. Retry later with exponential backoff.
    error_logger:error_msg("unavailable ~p~n", [RegId]),
    ok;

handle_error(<<"InternalServerError">>, RegId) ->
    % GCM had an internal server error. Retry later with exponential backoff.
    error_logger:error_msg("internal server error ~p~n", [RegId]),
    ok;

handle_error(<<"InvalidRegistration">>, RegId) ->
    %% Invalid registration id in database.
    error_logger:error_msg("invalid registration ~p~n", [RegId]),
    ok;

handle_error(<<"NotRegistered">>, RegId) ->
    %% Application removed. Delete device from database.
    error_logger:error_msg("not registered ~p~n", [RegId]),
    ok;

handle_error(UnexpectedError, RegId) ->
    %% There was an unexpected error that couldn't be identified.
    error_logger:error_msg("unexpected error ~p in ~p~n", [UnexpectedError, RegId]),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Other possible errors:					%%
%%	<<"InvalidPackageName">>				%%
%%      <<"MissingRegistration">>				%%
%%	<<"MismatchSenderId">>					%%
%%	<<"MessageTooBig">>					%%
%%      <<"InvalidDataKey">>					%%
%%	<<"InvalidTtl">>					%%
%%								%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
