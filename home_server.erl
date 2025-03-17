-module(home_server).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	home_server_sup:start_link().

stop(_State) ->
	ok.