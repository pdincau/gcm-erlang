-module(gcm_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

init_and_stop_test() ->
    ?assertMatch({ok, _}, gcm:start_link(test, "APIKEY")),
    ?assertEqual(stopped, gcm:stop(test)),
    ?assertEqual(undefined, whereis(test)).

gcm_message_test() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [{"it gets a 200 when sending a correct message", fun send_message_correct/1,
       "it gets a 400 when sending a message with a malformed json", fun send_message_wrong_json/1,
       "it gets a 401 when sending a message with wrong API key ", fun send_message_wrong_auth/1,
       "it gest a 503 when GMC servers are down", fun send_message_gcm_down/1
      }]}.

start() ->
    {ok, _} = gcm:start_link(test, "APIKEY"),
    meck:new(httpc),
    _Pid = self().
    
stop(_) ->
    meck:unload(httpc), 
    gcm:stop(test).

send_message_correct(Pid) ->
    meck:expect(httpc, request, 
		fun(post, {_BaseURL, _AuthHeader, "application/json", _JSON}, [], []) ->
			Reply = [{<<"multicast_id">>, <<"whatever">>},
				 {<<"success">>, 1},
				 {<<"results">>, [
						  [{<<"message_id">>, <<"1:0408">>}]
						 ]}
				],
			Pid ! {ok, {{"", 200, ""}, [], jsx:encode(Reply)}}
		end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive 
	Any -> ?assertMatch({ok, {{_,200,_}, [], _JSON}}, Any)
    end,
    ?assert(meck:validate(httpc)).

send_message_wrong_json(Pid) ->
    meck:expect(httpc, request, 
		fun(post, {_BaseURL, _AuthHeader, "application/json", _MalformedJSON}, [], []) ->
			Pid ! {ok, {{"", 400, ""}, [], []}}
		end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive 
        Any -> ?assertMatch({ok, {{_, 400, _}, [], []}}, Any)
    end,
    ?assert(meck:validate(httpc)).    

send_message_wrong_auth(Pid) ->
    meck:expect(httpc, request, 
		fun(post, {_BaseURL, _WrongAuthHeader, "application/json", _JSON}, [], []) ->
			Pid ! {ok, {{"", 401, ""}, [], []}}
		end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive 
        Any -> ?assertMatch({ok, {{_, 401, _}, [], []}}, Any)
    end,
    ?assert(meck:validate(httpc)).

send_message_gcm_down(Pid) ->
    meck:expect(httpc, request, 
		fun(post, {_BaseURL, _WrongAuthHeader, "application/json", _JSON}, [], []) ->
			Pid ! {ok, {{"", 503, ""}, [], []}}
		end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive 
        Any -> ?assertMatch({ok, {{_, 503, _}, [], []}}, Any)
    end,
    ?assert(meck:validate(httpc)).
