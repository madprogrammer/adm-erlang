%%%-------------------------------------------------------------------
%%% @author Sergey Anufrienko <sergey.anoufrienko@gmai.com>
%%% @copyright (C) 2015, Sergey Anufrienko
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2015 by Sergey Anufrienko <sergey.anoufrienko@gmail.com>
%%%-------------------------------------------------------------------
-module(adm).

-behaviour(gen_server).

%% API
-export([start/3, start/4, stop/1, start_link/3, start_link/4]).
-export([push/3, sync_push/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(OAUTHURL, "https://api.amazon.com/auth/O2/token").
-define(PUSHURL, "https://api.amazon.com/messaging/registrations/~s/messages").

-record(state, {client_id, client_secret, retry_after, error_fun, token}).

%%%===================================================================
%%% API
%%%===================================================================
start(Name, ClientID, ClientSecret) ->
    start(Name, ClientID, ClientSecret, fun handle_error/2).

start(Name, ClientID, ClientSecret, ErrorFun) ->
    adm_sup:start_child(Name, ClientID, ClientSecret, ErrorFun).

start_link(Name, ClientID, ClientSecret) ->
    start_link(Name, ClientID, ClientSecret, fun handle_error/2).

start_link(Name, ClientID, ClientSecret, ErrorFun) ->
    gen_server:start_link({local, Name}, ?MODULE, [ClientID, ClientSecret, ErrorFun], []).

stop(Name) ->
    gen_server:call(Name, stop).

push(Name, RegId, Message) ->
    gen_server:cast(Name, {send, RegId, Message}).

sync_push(Name, RegId, Message) ->
    gen_server:call(Name, {send, RegId, Message}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ClientID, ClientSecret, ErrorFun]) ->
    do_auth(#state{client_id=ClientID, client_secret=ClientSecret, retry_after=0, error_fun=ErrorFun}).

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({send, RegId, Message}, _From, State) ->
    case do_push(RegId, Message, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        Other ->
            {reply, Other, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({send, RegId, Message}, State) ->
    case do_push(RegId, Message, State) of
        {ok, NewState} ->
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_auth(#state{client_id=ClientID, client_secret=ClientSecret} = State) ->
    lager:info("Authenticating with Amazon using Client ID ~p~n", [ClientID]),
    AuthRequest = lists:flatten(io_lib:format("grant_type=client_credentials&scope=messaging:push&client_id=~s&client_secret=~s",
        [ClientID, ClientSecret])),
    try httpc:request(post, {?OAUTHURL, [], "application/x-www-form-urlencoded; charset=UTF-8", AuthRequest}, [], []) of
        {ok, {{_, 200, _}, _Headers, AuthResponse }} ->
            Json = jsonx:decode(response_to_binary(AuthResponse), [{format, proplist}]),
            AccessToken = proplists:get_value(<<"access_token">>, Json),
            {ok, State#state{token=AccessToken}};
        {error, Reason} ->
            {error, Reason};
        {ok, {{_, 401, _}, _, _}} ->
            {stop, authorization, unknown};
        {ok, {{_, Code, _}, _, _}} ->
            lager:error("Error response with status code ~p", [Code]),
            {noreply, unknown};
        OtherError ->
            lager:error("Other error: ~p~n", [OtherError]),
            {noreply, unknown}
    catch
        Exception ->
            {error, Exception}
    end.

do_push(RegId, Message, #state{error_fun = ErrorFun, token = Token, client_id = ClientID} = State) ->
    URL = lists:flatten(io_lib:format(?PUSHURL, [RegId])),
    Body = {[{<<"data">>, Message}]},
    JsonBody = jsonx:encode(Body),
    AuthHeader = string:concat("Bearer ", binary_to_list(Token)),
    try httpc:request(post, {URL, [
        {"Authorization", AuthHeader},
        {"X-Amzn-Type-Version", "com.amazon.device.messaging.ADMMessage@1.0"},
        {"Accept", "application/json"},
        {"X-Amzn-Accept-Type", "com.amazon.device.messaging.ADMSendResult@1.0"}
    ], "application/json", JsonBody}, [], []) of
        {ok, {{_, Code, _}, Headers, PushResponse }} ->
            Json = jsonx:decode(response_to_binary(PushResponse), [{format, proplist}]),
            case Code of
                200 ->
                    NewRegId = proplists:get_value(<<"registrationID">>, Json),
                    if
                        (RegId =/= NewRegId) ->
                            ErrorFun(<<"NewRegistrationId">>, {RegId, NewRegId});
                        true ->
                            ignore
                    end,
                    {ok, State};
                401 ->
                    % Request new access token and repeat request
                    case do_auth(State) of
                        {ok, NewState} ->
                            do_push(RegId, Message, NewState);
                        Other ->
                            Other
                    end;
                400 ->
                    % Some argument of the input was invalid
                    ErrorCode = proplists:get_value(<<"reason">>, Json),
                    ErrorFun(ErrorCode, {RegId, Message}),
                    {ok, State};
                413 ->
                    % Payload exceeded the maximum allowable size
                    ErrorFun(<<"MessageTooLarge">>, {RegId, Message}),
                    {ok, State};
                429 ->
                    % Maximum allowable rate of messages exceeded
                    lager:error("Maximum message rate exceeded by ~p~n", [ClientID]),
                    handle_retry(RegId, Message, Headers),
                    {ok, State};
                500 ->
                    lager:error("Internal server error, scheduling retry", []),
                    handle_retry(RegId, Message, Headers),
                    {ok, State};
                503 ->
                    lager:error("Server is temporarily unavailable, scheduling retry", []),
                    handle_retry(RegId, Message, Headers),
                    {ok, State};
                _ ->
                    {ok, State}
            end;
        {error, Reason} ->
            {error, Reason};
        OtherError ->
            lager:error("Other error: ~p~n", [OtherError]),
            {noreply, unknown}
    catch
        Exception ->
            {error, Exception}
    end.

handle_retry(RegId, Message, Headers) ->
    RetryAfter = proplists:get_value("retry-after", Headers),
    case string:to_integer(RetryAfter) of
        {error, _} ->
            ignore;
        {Int, _} ->
            erlang:send_after(Int * 1000, self(), {send, RegId, Message})
    end.

response_to_binary(Json) when is_binary(Json) ->
    Json;

response_to_binary(Json) when is_list(Json) ->
    list_to_binary(Json).

handle_error(Error, Arg) ->
    lager:error("adm error ~p, arg ~p~n", [Error, Arg]),
    ok.

