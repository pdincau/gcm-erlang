%%%-------------------------------------------------------------------
%%% @author Paolo D'Incau <paolo.dincau@gmail.com>
%%% @copyright (C) 2013, Paolo D'Incau
%%% @doc
%%%
%%% @end
%%% Created : 18 Apr 2013 by Paolo D'Incau <paolo.dincau@gmail.com>
%%%-------------------------------------------------------------------
-module(gcm).

-behaviour(gen_server).

%% API
-export([start/2, stop/1, start_link/2, push/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-define(BASEURL, "https://android.googleapis.com/gcm/send").
-define(TTL, 3600).
-define(COLLAPSE_KEY, <<"your_update">>).

-record(state, {key, retry_after}).

%%%===================================================================
%%% API
%%%===================================================================
start(Name, Key) ->
    gcm_sup:start_child(Name, Key).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name, Key) ->
    gen_server:start_link({local, Name}, ?MODULE, [Key], []).

stop(Name) ->
    gen_server:call(Name, stop).

push(Name, RegIds, Message) ->
    gen_server:cast(Name, {send, RegIds, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Key]) ->
    {ok, #state{key=Key, retry_after=0}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send, RegIds, Message}, #state{key=Key} = State) ->
    GCMRequest = build_gcm_request(Message, RegIds),
    ApiKey = string:concat("key=", Key),

    try httpc:request(post, {?BASEURL, [{"Authorization", ApiKey}], "application/json", GCMRequest}, [], []) of
        {ok, {{_, 200, _}, Headers, GCMResponse}} ->
            {struct, Json} = mochijson2_:decode(GCMResponse),
            {_Multicast, _Success, Failure, Canonical, Results} = get_response_fields(Json),
            case to_be_parsed(Failure, Canonical) of
                true ->
                    %% get retry-after header for future development
                    _RetryAfter = proplists:get_value("retry-after", Headers),
                    parse_results(Results, RegIds),
                    {noreply, State};
                false ->
                    {noreply, State}
            end;
        {error, Reason} ->
            %% Some general error during the request.
            %% Stop the gen_server
            {stop, Reason, State};
        {ok, {{_, 400, _}, _, _}} ->
            %% Some error in the Json.
            %% Keep on working.
            {noreply, State};
        {ok, {{_, 401, _}, _, _}} ->
            %% Some error in the authorization.
            %% Stop the gen_server.
            {stop, authorization, State};
        {ok, {{_StatusLine, _, _}, _, _Body}} ->
            %% Request handled but some error like timeout happened.
            %% keep on working.
            {noreply, State};
        OtherError ->
            %% Some other nasty error.
            {stop, OtherError, State}
    catch
        Exception ->
            %% Stop the gen_server.
            {stop, Exception, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
build_gcm_request(Message, RegIds) ->
    Struct = {struct, [{<<"registration_ids">>, RegIds}] ++ Message},
    iolist_to_binary(mochijson2_:encode(Struct)).

get_response_fields(Json) ->
    Multicast = proplists:get_value(<<"multicast_id">>, Json),
    Success = proplists:get_value(<<"success">>, Json),
    Failure = proplists:get_value(<<"failure">>, Json),
    Canonical = proplists:get_value(<<"canonical_ids">>, Json),
    Results = proplists:get_value(<<"results">>, Json),
    {Multicast, Success, Failure, Canonical, Results}.

to_be_parsed(Failure, Canonical) ->
    case {Failure, Canonical} of
        {0, 0} ->
            false;
        _ ->
            true
    end.

parse_results([Result|Results], [RegId|RegIds]) ->
    case Result of
        {struct, [{<<"error">>, Error}]} ->
            handle_error(Error, RegId),
            parse_results(Results, RegIds);
        {struct, [{<<"message_id">>, _Id}]} ->
	    io:format("Message sent.~n", []),
            parse_results(Results, RegIds);
        {struct, [{<<"message_id">>, _Id}, {<<"registration_id">>, NewRegId}]} ->
	    io:format("Message sent. Update id ~p with new id ~p.~n", [RegId, NewRegId]),
            parse_results(Results, RegIds);
        {struct, [{<<"registration_id">>, NewRegId}, {<<"message_id">>, _Id}]} ->
            io:format("Message sent. Update id ~p with new id ~p.~n", [RegId, NewRegId]),
            parse_results(Results, RegIds)
    end;

parse_results([], []) ->
    ok.

handle_error(<<"Unavailable">>, _RegId) ->
    %% The server couldn't process the request in time. Retry later with exponential backoff.
    io:format("Error: Unavailable.~n", []),
    ok;

handle_error(<<"InternalServerError">>, _RegId) ->
    % GCM had an internal server error. Retry later with exponential backoff.
    io:format("Error: Internal server error.~n", []),
    ok;

handle_error(<<"InvalidRegistration">>, _RegId) ->
    %% Invalid registration id in database. Should log.
    io:format("Error: Invalid registration.~n", []),
    ok;

handle_error(<<"NotRegistered">>, _RegId) ->
    %% Application removed. Delete device from database.
    io:format("Error: Not registered.~n", []),
    ok;

handle_error(UnexpectedError, _RegId) ->
    %% There was an unexpected error that couldn't be identified.
    io:format("Error: unexpected error ~p.~n", [UnexpectedError]),
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
