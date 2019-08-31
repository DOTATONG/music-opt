%%%-------------------------------------------------------------------
%%% Created : 2019-08-27 23:59
%%% Description : 
%%%-------------------------------------------------------------------
-module(music_tcp).

-behavior(gen_server).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

-include("common.hrl").

start_link(Port, DownPath) ->
	gen_server:start(?MODULE, [Port, DownPath], []).

%%%====================================
%%% gen_server回调方法
%%%====================================

init([Port, DownPath]) ->
	%停止tcp监听
	stop_server(Port),

	%启动tcp监听
	start_server(Port, DownPath),

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

start_server(Port, DownPath) ->
	Name = port_name(Port),
	case whereis(Name) of
		undefined ->
			Cid = self(),
			%spawn/1 不需要export start_listen
			%spawn/3 调用的函数必须要export出去
			Pid = spawn(fun() -> start_listen(Cid, Port, DownPath) end),
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

start_listen(Parent, Port, DownPath) ->
	process_flag(trap_exit, true),

	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, Listen} ->
			Parent ! {self(), ok},
			%接收客户端消息
			Pid = start_receive(Listen, DownPath),
			loop_receive(Listen, Pid, [], DownPath);
		Error ->
			Parent ! {self(), Error}

	end.

%接收客户端消息
start_receive(Listen, DownPath) ->
	Cid = self(),
	spawn(
		fun() ->
			case gen_tcp:accept(Listen) of
				{ok, Socket} ->
					Cid ! {started, self()},
					receive_callback(Socket, Cid, DownPath);
				_Other ->
					exit(Cid, _Other)
			end
		end).

loop_receive(Listen, Pid, ActiveList, DownPath) ->
%%	io:format("当前存活连接进程：~p~n", [ActiveList]),
	receive
		{started, Pid} ->
			check_process(Listen, no_pid, [Pid | ActiveList], DownPath);
		{'EXIT', Pid, _Reason} ->
			check_process(Listen, no_pid, lists:delete(Pid, ActiveList), DownPath);
		{'EXIT', ExitPid, _Reason} ->
			check_process(Listen, Pid, lists:delete(ExitPid, ActiveList), DownPath);
		Other ->
			io:format("Here in loop:~p~n", [Other]),
			loop_receive(Listen, Pid, ActiveList, DownPath)
	end.

check_process(Listen, Pid, ActiveList, DownPath) when is_pid(Pid) ->
	loop_receive(Listen, Pid, ActiveList, DownPath);
check_process(Listen, no_pid, ActiveList, DownPath) ->
	case length(ActiveList) of
		N when N < ?ALL_SERVER_PLAYERS ->
			NewPid = start_receive(Listen, DownPath),
			loop_receive(Listen, NewPid, ActiveList, DownPath);
		_ ->
			error_logger:warning_report([
				{module, ?MODULE},
				{method, ?FUNCTION_NAME},
				{line, ?LINE},
				{message, "已达最大连接数"},
				{maximum, ?ALL_SERVER_PLAYERS},
				{connected, length(ActiveList)}
			]),
			loop_receive(Listen, no_pid, ActiveList, DownPath)
	end.


%处理客户端消息
receive_callback(Socket, Cid, DownPath) ->
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
					websocket_callback(Socket, Cid, DownPath, 0, <<>>, 0)
			end;
		_ ->
			exit(Cid, <<"不支持的消息类型">>),
			gen_tcp:close(Socket)
	end.


websocket_callback(Socket, Cid, DownPath, MaskKey, Unmasked, UnmaskedLen) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Bin} ->
			case rfc6455:parse_data(Bin, MaskKey, UnmaskedLen) of
				{text, Data} ->
					{ok, List, _} = rfc4627:decode(Data),
					if
						is_list(List) ->
							exec_download(List, DownPath),
							gen_tcp:send(Socket, rfc6455:pack_data(<<"ok">>)),
							websocket_callback(Socket, Cid, DownPath, 0, <<>>, 0);
						true ->
							gen_tcp:send(Socket, rfc6455:pack_data(<<"格式错误">>)),
							exit(Cid, <<"消息格式错误">>),
							gen_tcp:close(Socket)
					end;
				{binary, DataBin} ->
					Payload = <<Unmasked/binary, DataBin/binary>>,
					{ok, List, _} = rfc4627:decode(Payload),
					if
						is_list(List) ->
							exec_download(List, DownPath),
							gen_tcp:send(Socket, rfc6455:pack_data(<<"ok">>)),
							websocket_callback(Socket, Cid, DownPath, 0, <<>>, 0);
						true ->
							gen_tcp:send(Socket, rfc6455:pack_data(<<"格式错误">>)),
							exit(Cid, <<"消息格式错误">>),
							gen_tcp:close(Socket)
					end;
				{more, Key, Payload, PayloadLen} ->
					websocket_callback(Socket, Cid, DownPath, Key, Payload, PayloadLen);
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

exec_download(List, DownPath) ->
	case filelib:is_dir(DownPath) of
		false ->
			file:make_dir(DownPath);
		_ ->
			nothing_to_do
	end,
	lists:foreach(
		fun(Item) ->
			[Id | [Txt | _End]] = Item,

			%%list过滤空格
%%			Txt1 = lists:foldl(fun(L, List) -> List ++ L end, [], string:replace(Txt, " ", "")),
%%			Name = lists:foldl(fun(L, List) -> List ++ L end, [], string:replace(Txt1, "/", "")),

			%%过滤空格[32]、nbsp空格[160]、"/"[47]、"\"[92]
%%			Name = [N || N <- unicode:characters_to_list(Txt), N /= 32, N /= 47, N /= 92],

			%%替换"/"[47]、"\"[92] 为 "_"[95]或"&"[38]
			Txt1 = binary:replace(Txt, <<47>>, <<95>>, [global]),
			Txt2 = binary:replace(Txt1, <<92>>, <<95>>, [global]),
			%%替换160为32会导致unicode:characters_to_list失败
%%			Txt3 = binary:replace(Txt2, <<160>>, <<32>>, [global]),
			Name = unicode:characters_to_list(Txt2),

			File = DownPath ++ "/" ++ Name ++ ".mp3",
			case filelib:is_file(File) of
				false ->
					os:cmd("wget -q -O \"" ++ File ++ "\" http://music.163.com/song/media/outer/url?id=" ++ binary_to_list(Id) ++ ".mp3");
				_ ->
					nothing_to_do
			end
		end, List),
	ok.