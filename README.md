adm-erlang
=======

[![Build Status](https://api.travis-ci.org/madprogrammer/adm-erlang.png)](https://travis-ci.org/madprogrammer/adm-erlang)

This software provides an Erlang client for [`AMAZON DEVICE MESSAGING`](https://developer.amazon.com/public/apis/engage/device-messaging "Amazon Device Messaging").

This software is based on [gcm-erlang](https://github.com/pdincau/gcm-erlang) by Paolo D'Incau.

## WARNING: This library is currently in alpha, operation is not guaranteed.

### What can you already do with adm-erlang:

Using `adm-erlang` you can:

1. Start several `gen_servers` representing different `ADM applications` defined by different `client id` and `client secret`
2. Send notification messages to Amazon mobile devices registered to your specific application and registered to `ADM` using a specific `registration id`

### How to compile the application adm-erlang:

The first thing you have to do is to compile all the Erlang files using `rebar`.

    $ ./rebar get-deps compile

### How to run the application adm-erlang:

Once all the Erlang files are compiled you can start the application `adm-erlang`. The application does use the module `httpc` so it is mandatory to  start also the Erlang application `inets`.

    $ erl -pa deps/*/ebin -pa ebin
    1> application:start(adm).
    ok

### How to start/stop different gen_servers under application adm-erlang (one for each ADM application):

While `adm-erlang` is running you can start several supervised gen_servers, one for each ADM application. Every gen_server is defined by an atom used internally for registration, as well as by a `client ID` and `client secret`.

    3> adm:start(foo, "myclientid", "myclientsecret").
    {ok,<0.60.0>}
    4> adm:start(bar, "myotherclientid", "myotherclientsecret").
    {ok,<0.65.0>}

You can stop a `gen_server` representing an ADM Application using:

    6> adm:stop(foo).

### How to send an ADM message from a specific ADM application:

At any time you can send an ADM message to one or more mobile devices by calling:

    7> adm:push(RegisteredName, RegId, Message).

or by calling:

    7> adm:sync_push(RegisteredName, RegId, Message).

Where `RegistereName` is the atom used during registration, `RegId` is the Registration ID specified as Erlang binary and `Message` is an Erlang term representing the data you want to send to the device.

The JSON message is built using `jsonx` in the module `adm.erl` and in the end will have the following form:

    {
      "data" : {
        "somekey1" : "value1",
        "somekey2" : "value2"
      }
    }

You can send this message using this sentence:

    8> adm:push(RegisteredName, RegIds, [
    8>     {<<"somekey1">>, <<"value1">>},
    8>     {<<"somekey2">>, <<"value2">>}
    8> ]).

`adm-erlang` will push the message for you to `Amazon Device Messaging` servers and will parse the JSON provided as result.

In case of errors you can catch the output with a callback function. You only need to start the adm gen_server in this way:

    9> Callback = fun(Error, RegId) -> io:format("~p ~p~n", [RegId, Error]) end.
    10> adm:start(foo, "clientID", "clientSecret", Callback).

The first param is always a binary with the error and the second param will be a tuple. All of the errors you can handle are in this list:

- `NewRegistrationId`, send the RegID and NewRegID as a tuple in the second parameter.

Following errors have a tuple of {RegId, Message} as the second parameter.

- `InvalidRegistrationId`, registration ID does not correspond to the sender identified by the provided access token.
- `InvalidData`, the supplied data was invalid.
- `InvalidConsolidationKey`, should not happen, as the library doesn't support consolidation keys yet.
- `InvalidExpiration`, should not happen, as the library doesn't support message expiration yet.
- `MessageTooLarge`, the message is too big (more than 6 KB).
- `Unregistered`, the app instance associated with the registration ID is no longer available to receive messages.

Read this for futher details see: [Response format](https://developer.amazon.com/appsandservices/apis/engage/device-messaging/tech-docs/06-sending-a-message#Response Format).

