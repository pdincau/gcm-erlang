-module(gcm_api).
-export([push/3]).

-define(BASEURL, "https://android.googleapis.com/gcm/send").

push(RegIds, Message, Key) ->
    Request = jsx:encode([{<<"registration_ids">>, RegIds}|Message]),
    ApiKey = string:concat("key=", Key),

    try httpc:request(post, {?BASEURL, [{"Authorization", ApiKey}], "application/json", Request}, [], []) of
        {ok, {{_, 200, _}, _Headers, Body}} ->
            Json = jsx:decode(response_to_binary(Body)),
            {ok, result_from(Json)};
        {error, Reason} ->
	    error_logger:error_msg("Error in request. Reason was: ~p~n", [Reason]),
            {error, Reason};
        {ok, {{_, 400, _}, _, _}} ->
	    error_logger:error_msg("Error in request. Reason was: json_error~n", []),
            {error, json_error};
        {ok, {{_, 401, _}, _, _}} ->
	    error_logger:error_msg("Error in request. Reason was: authorization error~n", []),
            {error, auth_error};
        {ok, {{_, Code, _}, Headers, _}} when Code >= 500 andalso Code =< 599 ->
	    RetryTime = retry_after_from(Headers),
	    error_logger:error_msg("Error in request. Reason was: retry. Will retry in: ~p~n", [RetryTime]),
            {error, {retry, RetryTime}};
        {ok, {{_StatusLine, _, _}, _, _Body}} ->
	    error_logger:error_msg("Error in request. Reason was: timeout~n", []),
            {error, timeout};
        OtherError ->
	    error_logger:error_msg("Error in request. Reason was: ~p~n", [OtherError]),
            {noreply, unknown}
    catch
        Exception ->
	    error_logger:error_msg("Error in request. Exception ~p while calling URL: ~p~n", [Exception, ?BASEURL]),
            {error, Exception}
    end.

response_to_binary(Json) when is_binary(Json) ->
    Json;

response_to_binary(Json) when is_list(Json) ->
    list_to_binary(Json).

result_from(Json) ->
    {
      proplists:get_value(<<"multicast_id">>, Json),
      proplists:get_value(<<"success">>, Json),
      proplists:get_value(<<"failure">>, Json),
      proplists:get_value(<<"canonical_ids">>, Json),
      proplists:get_value(<<"results">>, Json)
    }.

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
