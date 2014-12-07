-module(gcm_api_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

get_retry_after_when_http_date_test() ->
    application:start(qdate),
    Headers = [{"retry-after", "Fri, 31 Dec 1999 23:59:59 GMT"}],
    ?assertMatch({ok, I} when is_integer(I), gcm_api:retry_after_from(Headers)).

get_retry_after_when_seconds_interval_test() ->
    Headers = [{"retry-after", "120"}],
    ?assertEqual({ok, 120}, gcm_api:retry_after_from(Headers)).

get_retry_after_when_missing_test() ->
    Headers = [],
    ?assertEqual(no_retry, gcm_api:retry_after_from(Headers)).
