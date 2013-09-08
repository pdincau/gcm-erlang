gcm-erlang
=======

[![Build Status](https://api.travis-ci.org/pdincau/gcm-erlang.png)](https://travis-ci.org/pdincau/gcm-erlang)

This software provides an Erlang client for [`GOOGLE CLOUD MESSAGING`](http://developer.android.com/google/gcm/index.html "Google Cloud Messaging for Android").


### What can you already do with gcm-erlang:

Using `gcm-erlang` you can:

1. start several `gen_servers` representing different `GCM applications` defined by different `GCM API keys`
2. send notification messages to Android mobile devices registered to your specific application and registered to `GCM` using a specific `registration id`

So far `gcm-erlang` does only provide support for JSON messages since GCM does not allow to send multicast messages using plain text.

### How to compile the application gcm-erlang:

The first thing you have to do is to compile all the Erlang files using `rebar`.

    $ ./rebar get-deps compile

### How to run the application gcm-erlang:

Once all the Erlang files are compiled you can start the application `gcm-erlang`. The application does use the module `httpc` so it is mandatory to  start also the Erlang application `inets`.

    $ erl -pa deps/*/ebin -pa ebin
    1> application:start(gcm).
    ok

### How to start/stop different gen_servers under application gcm-erlang (one for each GCM application):

While `gcm-erlang` is running you can start several supervised gen_servers, one for each GCM application. Every gen_server is defined by an atom used internally for registration and by a `GCM API key`.

    3> gcm:start(foo, "myapikey").
    {ok,<0.60.0>}
    4> gcm:start(bar, "myotherapikey").
    {ok,<0.65.0>}
    5> gcm:start(baz, "mylastapikey").
    {ok,<0.79.0>}

You can stop a `gen_server` representing a GCM Application using:

    6> gcm:stop(foo).

### How to send a GCM message using from a specific GCM application:

At any time you can send a GCM message to one or more mobile devices by calling:

    7> gcm:push(RegisteredName, RegIds, Message).

Where `RegistereName` is the atom used during registration, `RegIds` is a list (max 1000 elements) of Registration Ids specified as Erlang binaries (e.g., `<<"APA91bHun4MxP5egoKMwt2KZFBaFUH-1RYqx...">>`) and `Message` is an Erlang term representing the data you want to send to the device.

The JSON message is built using `jsx` in the module `gcm.erl` and in the end will have the following form:

    {
      "registration_ids" : ["APA91bHun4MxP5egoKMwt2KZFBaFUH-1RYqx..."],
      "data" : {
        "message" : "a message"
      },
      "time_to_live" : 3600,
      "collapse_key" : "your_update"
    }

You can send this message using this sentence:

    8> gcm:push(RegisteredName, RegIds, [{<<"data">>, [
    8>     {<<"message">>, <<"a message">>}
    8> ]}, {<<"time_to_live">>,3600}, {<<"collapse_key">>,<<"your_update">>}]).

or simply:

    8> gcm:push(RegisteredName, RegIds, [{<<"data">>, [
    8>     {<<"message">>, <<"a message">>}
    8> ]}]).

`gcm-erlang` will push the message for you to `Google Cloud Messaging` servers and will parse the JSON provided as result.

In case of errors you can catch the output with a callback function. You only need to start the GCM gen_server in this way:

    9> Callback = fun(Error, RegId) -> io:format("~p ~p~n", [RegId, Error]) end.
    10> gcm:start(foo, "apikey", Callback).

The first param is always a binary with the error and the second param can be a binary with the Registered ID or a tuple refering to the Old Registered ID and the New Registered ID. All of the errors you can handle are in this list are:

- `NewRegistrationId`, send the OldRegID and NewRegID as a tuple in the second param.
- `Unavailable`, you should retry the request (use exponential back-off).
- `InternalServerError`, you should retry the request (use exponential back-off).
- `InvalidRegistration`, the RedID isn't valid, you should remove it from your database.
- `NotRegistered`, the RegID isn't valid, you should remove it from your database.
- `MissingRegistration` the RegID isn't valid, you should remove it from your database.
- `MessageTooBig`, the message is too big, so you should send another shorter.
- `InvalidDataKey`, the API KEY is invalid, you should stop the server and reconfigure it!
- `InvalidTtl`, the TTL (time to live) has an invalid value, stop and reconfigure it!
- `MismatchSenderId`, you miss send the sender id, drop the message.
- `InvalidPackageName`, package name invalid, drop the message.

Read this for futher details see: [Interpreting an error response](http://developer.android.com/google/gcm/gcm.html#response).

### Possible future improvements:

Some stuff I would like to add to `gcm-erlang`:

1. resending of all the message not sent due to `GCM` timeout using exponential backoff

In some cases when you receive a success/error message we should update/remove in our database the `Registration Ids`. I did not implement this because it depends on your needs. Problably I will store in different `ets tables` some information so that you will be able to inspect them and update your database. Feel free to edit in `gcm.erl` the functions `parse_results/2` and `handle_error/2` if you want to handle these issues in you own way.

### Note:

Some of the concepts I used for building this Erlang application are based on this [`blog post`](http://tiliman.wordpress.com/2013/01/02/google-cloud-messaging-with-erlang/) and on this [`Erlang application for APN`](https://github.com/extend/ex_apns).
