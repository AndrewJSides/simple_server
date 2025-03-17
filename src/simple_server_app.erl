%%%-------------------------------------------------------------------
%% @doc simple_server public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    % Example: Start a Cowboy server
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", my_handler, []}  % Replace with your routes
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}),
    {ok, self()}.  % Return a PID to keep the application running

stop(_State) ->
    cowboy:stop_listener(http_listener),
    ok.