-module(gcm_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

init_and_stop_test() ->
    ?assertMatch({ok, _}, gcm:start_link(test, "APIKEY")),
    ?assertEqual(stopped, gcm:stop(test)),
    ?assertEqual(undefined, whereis(test)).

start() ->
    {ok, _} = gcm:start_link(test, "APIKEY"),
    meck:new(httpc),
    _Pid = self().

stop(_) ->
    meck:unload(httpc),
    gcm:stop(test).

gcm_message_test_() ->
    [{"It gets a 200 when message is correct", ?setup(fun send_valid_message/1)},
     {"It gets a 400 when message contains malformed json", ?setup(fun send_malformed_json/1)},
     {"It gets a 401 when message has wrong auth", ?setup(fun send_wrong_auth/1)},
     {"It gets a 503 when GCM servers are down", ?setup(fun send_gcm_down/1)}].

send_valid_message(Pid) ->
    meck:expect(httpc, request,
		fun(post, {_BaseURL, _AuthHeader, "application/json", _JSON}, [], []) ->
			Reply = <<"{\"multicast_id\":\"whatever\",\"success\":1,\"results\":[{\"message_id\":\"1:0408\"}]}">>,
			Pid ! {ok, {{"", 200, ""}, [], Reply}}
		end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive
        Any -> [
                {"Status is 200", ?_assertMatch({ok, {{_,200,_}, [], _JSON}}, Any)},
                {"Validate httpc", ?_assert(meck:validate(httpc))}
               ]
    end.

send_malformed_json(Pid) ->
    meck:expect(httpc, request,
		fun(post, {_BaseURL, _AuthHeader, "application/json", _MalformedJSON}, [], []) ->
			Pid ! {ok, {{"", 400, ""}, [], []}}
		end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive
        Any -> [
                {"Status is 400", ?_assertMatch({ok, {{_, 400, _}, [], []}}, Any)},
                {"Validate httpc", ?_assert(meck:validate(httpc))}
               ]
    end.

send_wrong_auth(Pid) ->
    meck:expect(httpc, request,
		fun(post, {_BaseURL, _WrongAuthHeader, "application/json", _JSON}, [], []) ->
			Pid ! {ok, {{"", 401, ""}, [], []}}
		end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive
        Any -> [
                {"Status is 401", ?_assertMatch({ok, {{_, 401, _}, [], []}}, Any)},
                {"Validate httpc", ?_assert(meck:validate(httpc))}
               ]
    end.

send_gcm_down(Pid) ->
    meck:expect(httpc, request,
		fun(post, {_BaseURL, _WrongAuthHeader, "application/json", _JSON}, [], []) ->
			Pid ! {ok, {{"", 503, ""}, [], []}}
		end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive
        Any -> [
                {"Status is 503", ?_assertMatch({ok, {{_, 503, _}, [], []}}, Any)},
                {"Validate httpc", ?_assert(meck:validate(httpc))}
               ]
    end.
