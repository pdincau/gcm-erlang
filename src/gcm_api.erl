-module(gcm_api).
-export([push/3]).
-export([web_push/4]).

-define(BASEURL, "https://android.googleapis.com/gcm/send").
-define(TEMP_GCM_URL, "https://gcm-http.googleapis.com/gcm").

-type header()       :: {string(), string()}.
-type headers()      :: [header(),...].
-type regid()        :: binary().
-type regids()       :: [binary(),...].
-type message()      :: [tuple(),...] | binary().
-type result()       :: {number(), non_neg_integer(), non_neg_integer(), non_neg_integer(), [any()]}.
-type subscription() :: {regid(), webpush_encryption:publicKey(), webpush_encryption:authTokeny()}.

-spec push(regids(),message(),string()) -> {'error',any()} | {'noreply','unknown'} | {'ok',result()}.
push(RegIds, Message, Key) ->
    Request = jsx:encode([{<<"registration_ids">>, RegIds}|Message]),
    push(Request, Key, [], ?BASEURL).

-spec push(message(), string(), headers(), string()) -> {'error',any()} | {'noreply','unknown'} | {'ok',result()}.
push(Request, Key, Headers, BaseUrl) ->
    ApiKey = string:concat("key=", Key),
    try httpc:request(post, {BaseUrl, [{"Authorization", ApiKey}|Headers], "application/json", Request}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Json = jsx:decode(response_to_binary(Body)),
            error_logger:info_msg("Result was: ~p~n", [Json]),
            {ok, result_from(Json)};
        {ok, {{_, 201, _}, _Headers, _Body}} ->
            error_logger:info_msg("Result was: Created~n"),
            {ok, ok};
        {ok, {{_, 400, _}, _, Body}} ->
            error_logger:error_msg("Error in request. Reason was: Bad Request - ~p~n", [Body]),
            {error, Body};
        {ok, {{_, 401, _}, _, _}} ->
            error_logger:error_msg("Error in request. Reason was: authorization error~n", []),
            {error, auth_error};
        {ok, {{_, Code, _}, RespHeaders, _}} when Code >= 500 andalso Code =< 599 ->
            RetryTime = retry_after_from(RespHeaders),
            error_logger:error_msg("Error in request. Reason was: retry. Will retry in: ~p~n", [RetryTime]),
            {error, {retry, RetryTime}};
        {ok, {{StatusLine, RespHeaders, _}, _, _Body}} ->
            error_logger:error_msg("Error in request. Reason was: timeout1 ~p ~p ~n", [StatusLine, RespHeaders]),
            {error, timeout};
        {error, Reason} ->
            error_logger:error_msg("Error in request. Reason was: ~p~n", [Reason]),
            {error, Reason};
        OtherError ->
            error_logger:error_msg("Error in request. Reason other was: ~p~n", [OtherError]),
            {noreply, unknown}
    catch
        Exception ->
            error_logger:error_msg("Error in request. Exception ~p while calling URL: ~p~n", [Exception, BaseUrl]),
            {error, Exception}
    end.

-spec web_push(message(), string(), subscription(), non_neg_integer()) -> {'error',any()} | {'noreply','unknown'} | {'ok',result()}.
web_push(Message, Key, {RegId, ClientPublicKey, ClientAuthToken} = _Subscription, PaddingLength) ->
    {Ciphertext, Salt, PublicKey} = webpush_encryption:encrypt(jsx:encode(Message), {ClientPublicKey, ClientAuthToken}, PaddingLength),
    Headers = [
        {"Content-Encoding", "aesgcm"}
        , {"Encryption", "salt=" ++ binary_to_list( base64url:encode(Salt)) }
        , {"Crypto-Key", "dh=" ++ binary_to_list( base64url:encode(PublicKey)) }
        , {"TTL", "0"}
    ],
    PushServerUri = ?TEMP_GCM_URL ++ "/" ++ binary_to_list(RegId),
    push(Ciphertext, Key, Headers, PushServerUri).

-spec response_to_binary(binary() | list()) -> binary().
response_to_binary(Json) when is_binary(Json) ->
    Json;

response_to_binary(Json) when is_list(Json) ->
    list_to_binary(Json).

-spec result_from([{binary(),any()}]) -> result().
result_from(Json) ->
    {
      proplists:get_value(<<"multicast_id">>, Json),
      proplists:get_value(<<"success">>, Json),
      proplists:get_value(<<"failure">>, Json),
      proplists:get_value(<<"canonical_ids">>, Json),
      proplists:get_value(<<"results">>, Json)
    }.

-spec retry_after_from(headers()) -> 'no_retry' | non_neg_integer().
retry_after_from(Headers) ->
    case proplists:get_value("retry-after", Headers) of
        undefined ->
            no_retry;
        RetryTime ->
            case string:to_integer(RetryTime) of
                {Time, _} when is_integer(Time) ->
                    Time;
                {error, no_integer} ->
                    Date = qdate:to_unixtime(RetryTime),
                    Date - qdate:unixtime()
            end
    end.
