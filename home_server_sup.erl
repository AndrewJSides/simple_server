-module(home_server_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 5, 10}, [
		{home_server,
		 {simple_server, start, []},
		 permanent, 5000, worker, [simple_server]}
	]}}.