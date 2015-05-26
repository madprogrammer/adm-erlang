%%%-------------------------------------------------------------------
%%% @author Sergey Anufrienko <sergey.anoufrienko@gmai.com>
%%% @copyright (C) 2015, Sergey Anufrienko
%%% @doc
%%%
%%% @end
%%% Created : 22 May 2015 by Sergey Anufrienko <sergey.anoufrienko@gmail.com>
%%%-------------------------------------------------------------------
-module(adm_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/4]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

start_child(Name, ClientID, ClientSecret, ErrorFun) ->
    supervisor:start_child(?MODULE, [Name, ClientID, ClientSecret, ErrorFun]).

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [?CHILD(adm, worker)]} }.

