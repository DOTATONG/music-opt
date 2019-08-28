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

	case application:get_env(path) of
		{ok, EnvPath} ->
			DownPath = EnvPath;
		undefined ->
			{ok, NowPath} = file:get_cwd(),
			DownPath = [NowPath, "/download"]
	end,
	music_sup:start_link([Port, DownPath]).

stop(_State) ->
	ok.

%% internal functions
