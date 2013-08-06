-module(gcm_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

init_and_stop_test() ->
    ?assertMatch({ok, _}, gcm:start_link(test, "APIKEY")),
    ?assertEqual(stopped, gcm:stop(test)),
    ?assertEqual(undefined, whereis(test)).

gcm_simple_message_test() ->
    {ok, _} = gcm:start_link(test, "APIKEY"),
    PID = self(),
    meck:new(httpc),
    meck:expect(httpc, request, fun
        (post, {_BaseURL, _AuthHeader, "application/json", JSON}, [], []) ->
            Reply = [
                {<<"multicast_id">>, <<"whatever">>},
                {<<"success">>, 1},
                {<<"results">>, [
                    [{<<"message_id">>, <<"1:0408">>}]
                ]}
            ],
            PID ! {ok, {{"", 200, ""}, [], jsx:encode(Reply)}}
    end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive 
        Any -> ?assertMatch({ok, {{_,200,_}, [], _JSON}}, Any)
    end,
    meck:unload(httpc), 
    gcm:stop(test),
    ok.

gcm_simple_error401_message_test() ->
    {ok, _} = gcm:start_link(test, "APIKEY"),
    PID = self(),
    meck:new(httpc),
    meck:expect(httpc, request, fun
        (post, {_BaseURL, _AuthHeader, "application/json", JSON}, [], []) ->
            PID ! {ok, {{"", 401, ""}, [], []}}
    end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive 
        Any -> ?assertMatch({ok, {{_,401,_}, [], []}}, Any)
    end,
    meck:unload(httpc), 
    gcm:stop(test),
    ok.

gcm_simple_error400_message_test() ->
    {ok, _} = gcm:start_link(test, "APIKEY"),
    PID = self(),
    meck:new(httpc),
    meck:expect(httpc, request, fun
        (post, {_BaseURL, _AuthHeader, "application/json", JSON}, [], []) ->
            PID ! {ok, {{"", 400, ""}, [], []}}
    end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive 
        Any -> ?assertMatch({ok, {{_,400,_}, [], []}}, Any)
    end,
    meck:unload(httpc), 
    gcm:stop(test),
    ok.

gcm_simple_error503_message_test() ->
    {ok, _} = gcm:start_link(test, "APIKEY"),
    PID = self(),
    meck:new(httpc),
    meck:expect(httpc, request, fun
        (post, {_BaseURL, _AuthHeader, "application/json", JSON}, [], []) ->
            PID ! {ok, {{"", 503, ""}, [], []}}
    end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive 
        Any -> ?assertMatch({ok, {{_,503,_}, [], []}}, Any)
    end,
    meck:unload(httpc), 
    gcm:stop(test),
    ok.

gcm_simple_processed_message_test() ->
    {ok, _} = gcm:start_link(test, "APIKEY"),
    PID = self(),
    meck:new(httpc),
    meck:expect(httpc, request, fun
        (post, {_BaseURL, _AuthHeader, "application/json", JSON}, [], []) ->
            PID ! {ok, {{"", 203, ""}, [], []}}
    end),
    gcm:push(test, [<<"Token">>], [{<<"data">>, [{<<"type">>, <<"wakeUp">>}]}]),
    receive 
        Any -> ?assertMatch({ok, {{_,203,_}, [], []}}, Any)
    end,
    meck:unload(httpc), 
    gcm:stop(test),
    ok.
