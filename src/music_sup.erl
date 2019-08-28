%%%-------------------------------------------------------------------
%% @doc music top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(music_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

start_link([Port]) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).


%%	{ok,
%%		{
%%			#{
%%				strategy => strategy(),         	% optional
%%   			intensity => non_neg_integer(), 	% optional
%%   			period => pos_integer()        		% optional
%%			},
%%			[
%% 				#{
%%					id => child_id(),       	   	% mandatory
%%   				start => mfargs(),      		% mandatory
%%   				restart => restart(),   		% optional
%%   				shutdown => shutdown(), 		% optional
%%   				type => worker(),       		% optional
%%   				modules => modules()	   		% optional
%%				}
%% 			]
%%		}
%%	}
init([Port]) ->
	{ok,
		{
			{one_for_one, 3, 10},
			[
				{
					music_tcp,
					{music_tcp, start_link, [Port]},
					permanent,
					10000,
					supervisor,
					[music_tcp]
				}
			]
		}
	}.
