-module(adm_test).
-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(setup(F), {setup, fun start/0, fun stop/1, F}).

init_and_stop_test() ->
    true.

start() ->
    meck:new(httpc),
    meck:expect(httpc, request,
		fun(post, {"https://api.amazon.com/auth/O2/token", _AuthHeader, "application/x-www-form-urlencoded; charset=UTF-8", _JSON}, [], []) ->
			Reply = <<"{\"access_token\":\"whatever\"}">>,
			self() ! {ok, {{"", 200, ""}, [], Reply}}
		end),
    {ok, _} = adm:start_link(test, "clientID", "clientSecret"),
    _Pid = self().

stop(_) ->
    meck:unload(httpc),
    adm:stop(test).

adm_message_test_() ->
    [
     {"It gets a 200 when message is correct", ?setup(fun send_valid_message/1)},
     {"It gets a 400 when message contains malformed json", ?setup(fun send_malformed_json/1)},
     {"It gets a 401 when message has wrong auth", ?setup(fun send_wrong_auth/1)},
     {"It gets a 503 when adm servers are down", ?setup(fun send_adm_down/1)},
     {"It returns an error when there is no connection to adm servers", ?setup(fun send_no_connection/1)}
    ].

send_valid_message(Pid) ->
    meck:expect(httpc, request,
		fun(post, {_BaseURL, _AuthHeader, "application/json", _JSON}, [], []) ->
			Reply = <<"{\"registrationID\":\"1\"}">>,
			Pid ! {ok, {{"", 200, ""}, [], Reply}}
		end),
    adm:push(test, <<"Token">>, [{<<"type">>, <<"wakeUp">>}]),
    receive
        Any -> [
                {"Status is 200", ?_assertMatch({ok, {{_,200,_}, [], _JSON}}, Any)},
                {"Validate httpc", ?_assert(meck:validate(httpc))}
               ]
    end.

send_malformed_json(Pid) ->
    meck:expect(httpc, request,
		fun(post, {_BaseURL, _AuthHeader, "application/json", _MalformedJSON}, [], []) ->
			Pid ! {ok, {{"", 400, ""}, [], "{\"reason\":\"InvalidData\"}"}}
		end),
    adm:push(test, <<"Token">>, [{<<"type">>, <<"wakeUp">>}]),
    receive
        Any -> [
                {"Status is 400", ?_assertMatch({ok, {{_, 400, _}, [], _JSON}}, Any)},
                {"Validate httpc", ?_assert(meck:validate(httpc))}
               ]
    end.

send_wrong_auth(Pid) ->
    meck:expect(httpc, request,
		fun(post, {URL, Headers, _ContentType, _JSON}, [], []) ->
            case URL of
                "https://api.amazon.com/auth/O2/token" ->
                    {ok, {{"", 200, ""}, [], <<"{\"access_token\":\"newtoken\"}">>}};
                _ ->
                    case proplists:get_value("Authorization", Headers) of
                        "Bearer whatever" ->
                            Pid ! {ok, {{"", 401, ""}, [], []}};
                        "Bearer newtoken" ->
                            Pid ! {ok, {{"", 200, ""}, [], "{\"registrationID\":\"whatever\"}"}}
                    end
            end
		end),
    Res = adm:push(test, <<"Token">>, [{<<"type">>, <<"wakeUp">>}]),
    receive
        Any -> Any
    end,
    receive
        Any2 -> [
                {"Status is 401", ?_assertMatch({ok, {{_, 401, _}, [], _JSON}}, Any)},
                {"Status is 200", ?_assertMatch({ok, {{_, 200, _}, [], _JSON}}, Any2)},
                {"Validate httpc", ?_assert(meck:validate(httpc))},
                {"Send result is ok", ?_assertMatch(ok, Res)}
               ]
    end.

send_adm_down(Pid) ->
    meck:expect(httpc, request,
		fun(post, {_BaseURL, _WrongAuthHeader, "application/json", _JSON}, [], []) ->
			Pid ! {ok, {{"", 503, ""}, [{"retry-after", "10"}], []}}
		end),
    adm:push(test, <<"Token">>, [{<<"type">>, <<"wakeUp">>}]),
    receive
        Any -> [
                {"Status is 503", ?_assertMatch({ok, {{_, 503, _}, _Headers, []}}, Any)},
                {"Validate httpc", ?_assert(meck:validate(httpc))}
               ]
    end.

send_no_connection(Pid) ->
    meck:expect(httpc, request,
		fun(post, {_BaseURL, _WrongAuthHeader, "application/json", _JSON}, [], []) ->
			{error,{failed_connect,[{to_address,{"api.amazon.com",443}}, {inet,[inet],nxdomain}]}}
		end),
    Res = adm:sync_push(test, <<"Token">>, [{<<"type">>, <<"wakeUp">>}]),
    [
     {"Result is error", ?_assertMatch({error, {failed_connect, _}}, Res)},
     {"Validate httpc", ?_assert(meck:validate(httpc))}
    ].

