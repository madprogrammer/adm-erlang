%%%-------------------------------------------------------------------
%%% @author Sergey Anufrienko <sergey.anoufrienko@gmai.com>
%%% @copyright (C) 2015, Sergey Anufrienko
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2015 by Sergey Anufrienko <sergey.anoufrienko@gmail.com>
%%%-------------------------------------------------------------------
-module(adm_app).

-behaviour(application).

%% Application callbacks
-export([start/0]).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:start(inets),
    application:start(lager),
    application:start(adm).

start(_StartType, _StartArgs) ->
    adm_sup:start_link().

stop(_State) ->
    ok.
