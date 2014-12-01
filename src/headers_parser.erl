-module(headers_parser).
-export([retry_after_from/1]).

retry_after_from(Headers) ->
    case proplists:get_value("retry-after", Headers) of
	undefined ->
	    no_retry;
	RetryTime ->
	    case string:to_integer(RetryTime) of
		{Time, _} when is_integer(Time) ->
		    {ok, Time};
		{error,no_integer} ->
		    Date = qdate:to_unixtime(RetryTime),
		    {ok, Date - qdate:unixtime()}
	    end
    end.
