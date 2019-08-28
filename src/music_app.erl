%%%-------------------------------------------------------------------
%% @doc music public API
%% @end
%%%-------------------------------------------------------------------

-module(music_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	{ok, EnvPort} = application:get_env(port),
	if
		is_list(EnvPort) ->
			Port = list_to_integer(EnvPort);
		true ->
			Port = EnvPort
	end,
	music_sup:start_link([Port]).

stop(_State) ->
	ok.

%% internal functions
