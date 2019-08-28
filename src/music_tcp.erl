%%%-------------------------------------------------------------------
%%% Created : 2019-08-27 23:59
%%% Description : 
%%%-------------------------------------------------------------------
-module(music_tcp).

-behavior(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-include("common.hrl").

start_link(Port) ->
	gen_server:start(?MODULE, [Port], []).

%%%====================================
%%% gen_server回调方法
%%%====================================

init([Port]) ->
	%停止tcp监听
	stop_server(Port),

	%启动tcp监听
	start_server(Port),

	{ok, true}.

handle_call(_Request, _From, State) ->

	{ok, State}.

handle_cast(_Request, State) ->

	{ok, State}.

terminate(_Reason, State) ->
	{ok, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%========================================
%%% module 内部方法
%%%========================================

start_server(Port) ->
	Name = port_name(Port),
	case whereis(Name) of
		undefined ->
			Cid = self(),
			%spawn/1 不需要export start_listen
			%spawn/3 调用的函数必须要export出去
			Pid = spawn(fun() -> start_listen(Cid, Port) end),
			receive
				{Pid, ok} ->
					register(Name, Pid);
				{Pid, Error} ->
					Error
			end;
		_Pid ->
			{error, already_started}
	end.

stop_server(Port) when is_integer(Port) ->
	Name = port_name(Port),
	case whereis(Name) of
		undefined ->
			not_started;
		Pid ->
			exit(Pid, kill),
			(catch unregister(Name)),
			stopped
	end.

%%%========================================
%%% module 内部方法
%%%========================================

port_name(Port) when is_integer(Port) ->
	list_to_atom("music_" ++ integer_to_list(Port)).

start_listen(Parent, Port) ->
	process_flag(trap_exit, true),

	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, Listen} ->
			Parent ! {self(), ok},
			%接收客户端消息
			Pid = start_receive(Listen),
			loop_receive(Listen, Pid, []);
		Error ->
			Parent ! {self(), Error}

	end.

%接收客户端消息
start_receive(Listen) ->
	Cid = self(),
	spawn(
		fun() ->
			case gen_tcp:accept(Listen) of
				{ok, Socket} ->
					Cid ! {started, self()},
					receive_callback(Socket, Cid);
				_Other ->
					exit(Cid, _Other)
			end
		end).

loop_receive(Listen, Pid, ActiveList) ->
%%	io:format("当前存活连接进程：~p~n", [ActiveList]),
	receive
		{started, Pid} ->
			check_process(Listen, no_pid, [Pid | ActiveList]);
		{'EXIT', Pid, _Reason} ->
			check_process(Listen, no_pid, lists:delete(Pid, ActiveList));
		{'EXIT', ExitPid, _Reason} ->
			check_process(Listen, Pid, lists:delete(ExitPid, ActiveList));
		Other ->
			io:format("Here in loop:~p~n", [Other]),
			loop_receive(Listen, Pid, ActiveList)
	end.

check_process(Listen, Pid, ActiveList) when is_pid(Pid) ->
	loop_receive(Listen, Pid, ActiveList);
check_process(Listen, no_pid, ActiveList) ->
	case length(ActiveList) of
		N when N < ?ALL_SERVER_PLAYERS ->
			NewPid = start_receive(Listen),
			loop_receive(Listen, NewPid, ActiveList);
		_ ->
			error_logger:warning_report([
				{module, ?MODULE},
				{method, ?FUNCTION_NAME},
				{line, ?LINE},
				{message, "已达最大连接数"},
				{maximum, ?ALL_SERVER_PLAYERS},
				{connected, length(ActiveList)}
			]),
			loop_receive(Listen, no_pid, ActiveList)
	end.


%处理客户端消息
receive_callback(Socket, Cid) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Bin} ->
			%匹配websocket连接请求
			case string:find(binary_to_list(Bin), "Upgrade: websocket") of
				nomatch ->
					exit(Cid, <<"不支持的消息类型">>),
					gen_tcp:close(Socket);
				_ ->
%%					io:format("~p收到WebSocket连接~n", [self()]),
					gen_tcp:send(Socket, rfc6455:reply(Bin)),
					websocket_callback(Socket, Cid)
			end;
		_ ->
			exit(Cid, <<"不支持的消息类型">>),
			gen_tcp:close(Socket)
	end.


websocket_callback(Socket, Cid) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Bin} ->
			case rfc6455:parse_data(Bin) of
				{text, Data} ->
					{ok, List, _} = rfc4627:decode(Data),
					if
						is_list(List) ->
							exec_download(List),
							gen_tcp:send(Socket, rfc6455:pack_data(<<"ok">>)),
							websocket_callback(Socket, Cid);
						true ->
							gen_tcp:send(Socket, rfc6455:pack_data(<<"格式错误">>)),
							exit(Cid, <<"消息格式错误">>),
							gen_tcp:close(Socket)
					end;
				close ->
%%					io:format("~p收到WebSocket关闭连接消息~n", [self()]),
					exit(Cid, <<"客户端关闭连接">>),
					gen_tcp:close(Socket);
				error ->
%%					io:format("~p消息无法解析", [self()]),
					exit(Cid, <<"消息无法解析">>),
					gen_tcp:close(Socket)
			end;
		_ ->
			exit(Cid, <<"不支持的消息类型">>),
			gen_tcp:close(Socket)
	end.

exec_download(List) ->
	case filelib:is_dir(<<"./download">>) of
		false ->
			file:make_dir(<<"./download">>);
		_ ->
			nothing_to_do
	end,
	lists:foreach(
		fun(Item) ->
			[Id | Str] = Item,
			Name = [N || N <- string:replace(Str, " ", ""), N /= []],
			case filelib:is_file(["./download/", Name]) of
				false ->
					CmdBin = list_to_binary(["wget -q -O ./download/", Name, ".mp3 http://music.163.com/song/media/outer/url?id=", Id, ".mp3"]),
					os:cmd(unicode:characters_to_list(CmdBin));
				_ ->
					nothing_to_do
			end
		end, List),
	ok.