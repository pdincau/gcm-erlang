-module(gcm).
-behaviour(gen_server).

-export([start/2, stop/1, start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-export([push/3, sync_push/3]).

-define(SERVER, ?MODULE).

-record(state, {key}).

start(Name, Key) ->
    gcm_sup:start_child(Name, Key).

stop(Name) ->
    gen_server:call(Name, stop).

push(Name, RegIds, Message) ->
    gen_server:cast(Name, {send, RegIds, Message}).

sync_push(Name, RegIds, Message) ->
    gen_server:call(Name, {send, RegIds, Message}).

%% OTP
start_link(Name, Key) ->
    gen_server:start_link({local, Name}, ?MODULE, [Key], []).

init([Key]) ->
    {ok, #state{key=Key}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({send, RegIds, Message}, _From, #state{key=Key} = State) ->
    Reply = do_push(RegIds, Message, Key),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({send, RegIds, Message}, #state{key=Key} = State) ->
    do_push(RegIds, Message, Key),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal
do_push(RegIds, Message, Key) ->
    error_logger:info_msg("Sending message: ~p to reg ids: ~p~n", [Message, RegIds]),
    case gcm_api:push(RegIds, Message, Key) of
        {ok, GCMResult} ->
            handle_result(GCMResult, RegIds);
        {error, {retry, RetryTime}} ->
            do_backoff(RetryTime, RegIds, Message, Key),
            {error, retry};
        {error, Reason} ->
            {error, Reason}
    end.

handle_result(GCMResult, RegIds) ->
    {_MulticastId, _SuccessesNumber, _FailuresNumber, _CanonicalIdsNumber, Results} = GCMResult,
    lists:map(fun({Result, RegId}) ->
		      parse_result(Result, RegId)
	      end, lists:zip(Results, RegIds)).

do_backoff(RetryTime, RegIds, Message, Key) ->
    case RetryTime of
        no_retry ->
            ok;
	Time ->
	    timer:apply_after(Time * 1000, ?MODULE, do_push, [RegIds, Message, Key])
    end.

parse_result(Result, RegId) ->
    case {
      proplists:get_value(<<"error">>, Result),
      proplists:get_value(<<"message_id">>, Result),
      proplists:get_value(<<"registration_id">>, Result)
     } of
        {Error, undefined, undefined} when Error =/= undefined ->
            error_logger:info_msg("Error: ~p for registered id: ~p~n", [Error, RegId]),
            {RegId, Error};
        {undefined, MessageId, undefined} when MessageId =/= undefined ->
            ok;
        {undefined, MessageId, NewRegId} when MessageId =/= undefined andalso NewRegId =/= undefined ->
            error_logger:info_msg("Message sent. Update id ~p with new id ~p.~n", [RegId, NewRegId]),
            {RegId, {<<"NewRegistrationId">>, NewRegId}}
    end.
