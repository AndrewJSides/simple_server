%%%-------------------------------------------------------------------
%% @doc simple_server public API
%% @end
%%%-------------------------------------------------------------------

-module(simple_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/erl/hello", simple_server, hello},
            {"/", simple_server, index}
        ]}
    ]),

    Port = list_to_integer(os:getenv("PORT", "8080")),
    case cowboy:start_clear(http_listener,
                            [{port, Port}],
                            #{env => #{dispatch => Dispatch}}) of
        {ok, _Pid} ->
            io:format("Cowboy started on port ~p~n", [Port]),
            {ok, self()};
        {error, Reason} ->
            io:format("Failed to start Cowboy: ~p~n", [Reason]),
            {error, Reason}
    end.

stop(_State) ->
    cowboy:stop_listener(http_listener),
    ok.