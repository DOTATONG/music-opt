%%%-------------------------------------------------------------------
%%% Created : 2019-08-18 22:45
%%% Description : 公共定义
%%%-------------------------------------------------------------------
-define(ALL_SERVER_PLAYERS, 50).

%%tcp_server监听参数
-define(TCP_OPTIONS, [
	binary,
	{packet, 0},
	{active, false},
	{reuseaddr, true},
	{nodelay, false},
	{delay_send, true},
	{send_timeout, 5000},
	{keepalive, true},
	{exit_on_close, true}
]).
