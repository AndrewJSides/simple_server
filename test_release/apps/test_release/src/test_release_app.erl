%%%-------------------------------------------------------------------
%% @doc test_release public API
%% @end
%%%-------------------------------------------------------------------

-module(test_release_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    test_release_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
