gcm-erlang
=======

This software provides an Erlang client for [`GOOGLE CLOUD MESSAGING`](http://developer.android.com/google/gcm/index.html "Google Cloud Messaging for Android").


### What can you already do with gcm-erlang:

Using `gcm-erlang` you can:

1. start several `gen_servers` representing different `GCM applications` defined by different `GCM API keys`
2. send notification messages to Android mobile devices registered to your specific application and registered to `GCM` using a specific `registration id`

So far `gcm-erlang` does only provide support for JSON messages since GCM does not allow to send multicast messages using plain text.

### How to compile the application gcm-erlang:

The first thing you have to do is to compile all the Erlang files using `rebar`.
    
    $ ./rebar compile

### How to run the application gcm-erlang:

Once all the Erlang files are compiled you can start the application `gcm-erlang`. The application does use the module `httpc` so it is mandatory to  start also the Erlang application `inets`.

    $ erl -pa ebin
    1> application:start(inets).
    ok
    2> application:start(gcm).
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

Where `RegistereName` is the atom used during registration, `RegIds` is a list (max 1000 elements) of Registration Ids specified as Erlang binaries and `Message` is an Erlang binary representing the actual message. 

The JSON message is built using `mochijson2.erl` in the module `gcm.erl` and in the end will have the following form:

    {
      "registration_ids" : ["APA91bHun4MxP5egoKMwt2KZFBaFUH-1RYqx..."],
      "data" : {
        "message" : "the message you passed as argument"
      },
      "time_to_live" : 3600,
      "collapse_key" : "your_update"
    }

By looking at the code in `gcm.erl` you can easily change the JSON in order to satisfy your needs.

`gcm-erlang` will push the message for you to `Google Cloud Messaging` servers and will parse the JSON provided as result.

In case of errors or if some of the `Registration Ids` must be updated `gcm-erlang` will print a message on the standard output. I will add some better logging in the future (problably using `Lager` but other suggestions are more than accepted).

### Possible future improvements:

Some stuff I would like to add to `gcm-erlang`:

1. better logging
2. improved and adaptive construction of the JSON messages
3. resending of all the message not sent due to `GCM` timeout using exponential backoff

In some cases when you receive a success/error message we should update/remove in our database the `Registration Ids`. I did not implement this because it depends on your needs. Problably I will store in different `ets tables` some information so that you will be able to inspect them and update your database. Feel free to edit in `gcm.erl` the functions `parse_results/2` and handle_error/2 if you want to handle the error in you own way.

### Note:

Some of the concepts I used for building this Erlang application are based on this [`blog post`](http://tiliman.wordpress.com/2013/01/02/google-cloud-messaging-with-erlang/) and on this [`Erlang application for APN`](https://github.com/extend/ex_apns).
