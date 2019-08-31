%%%-------------------------------------------------------------------
%%% Sourced : https://github.com/HugoRun/websocket-demo
%%% Created : 2019-08-23 04:49
%%% Description : RFC 6455 WebSocket协议解析
%%%-------------------------------------------------------------------
-module(rfc6455).

-export([reply/1, parse_data/3, pack_data/1, pack_data/2]).

%%打包返回数据
pack_data(Payload) ->
	pack_data(Payload, text).

pack_data(Payload, text) when is_number(Payload) ->
	integer_to_list(Payload);
pack_data(Payload, text) ->
	Len = iolist_size(Payload),
	BinLen = payload_length_to_binary(Len),
	[<<8:4, 1:4, 0:1, BinLen/bits>>, Payload];
pack_data(Payload, binary) ->
	Len = iolist_size(binary_to_list(Payload)),
	BinLen = payload_length_to_binary(Len),
	<<8:4, 2:4, 0:1, BinLen/bits, Payload/bits>>.

%%
payload_length_to_binary(N) ->
	case N of
		N when N =< 125 -> <<N:7>>;
		N when N =< 16#ffff -> <<126:7, N:16>>;
		N when N =< 16#7fffffffffffffff -> <<127:7, N:64>>
	end.

%%  WebSocket解析
%%  https://tools.ietf.org/html/rfc6455
%%  格式按位说明
%%  0:Fin, 消息是否结束 1已结束 0未结束
%%  1:RSV1, 用于扩展定义, 若无则必须为0
%%  2:RSV2, 用于扩展定义, 若无则必须为0
%%  3:RSV3, 用于扩展定义, 若无则必须为0
%%  4-7:OPCODE, 消息接受类型, 若接受到位置的OPCODE, 接受端必须关闭连接
%%      0x0表示附加数据帧
%%      0x1表示文本数据帧
%%      0x2表示二进制数据帧
%%      0x3-7暂时无定义，为以后的非控制帧保留
%%      0x8表示连接关闭
%%      0x9表示ping
%%      0xA表示pong
%%      0xB-F暂时无定义，为以后的控制帧保留
%%  8:MASK, 用于标识Payload Data是否经过掩码处理，客户端发出的数据帧需要进行掩码处理，所以此位是1。数据需要解码
%%  9-n:Payload Len
%%      占位:7位、7+16位或7+64位
%%      若取值0-125, 则是payload的真实长度
%%      若取值126, 则后面2个字节形成的16位无符号整型数的值是payload的真实长度。注意，网络字节序，需要转换
%%      若取值127, 则后面8个字节形成的64位无符号整型数的值是payload的真实长度。注意，网络字节序，需要转换。
%%  n-:消息体

%%  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%%  +-+-+-+-+-------+-+-------------+-----------------------------+
%%  |F|R|R|R| opcode|M| Payload len |   Extended payload length   |
%%  |I|S|S|S|  (4)  |A|     (7)     |            (16/64)          |
%%  |N|V|V|V|       |S|             |  (if payload len==126/127)  |
%%  | |1|2|3|       |K|             |                             |
%%  +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - +
%%  |  Extended payload length continued, if payload len == 127   |
%%  + - - - - - - - - - - - - - - - +-----------------------------+
%%  |                               | Mask-key, if MASK set to 1  |
%%  +-------------------------------+-----------------------------+
%%  | Mask-key (continued)          |          Payload Data       |
%%  +-------------------------------- - - - - - - - - - - - - - - +
%%  :                     Payload Data continued ...              :
%%  + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
%%  |                     Payload Data continued ...              |
%%  +-------------------------------------------------------------+

%%解析客户端数据
parse_data(Data, _, _) when is_list(Data) ->
	parse_data(list_to_binary(Data), 0, 0);
parse_data(<<8:4, 0:4, _/bits>>, _, _) ->
	continue;
parse_data(<<8:4, 8:4, _/bits>>, _, _) ->
	close;
parse_data(<<8:4, Opcode:4, 1:1, Len:7, MaskKey:32, Rest/bits>>, _, _) when Len < 126, Opcode < 3, Opcode > 0 ->
	return_rest(Opcode, parse_rest(Len, MaskKey, Rest, 0));
parse_data(<<8:4, Opcode:4, 1:1, 126:7, Len:16, MaskKey:32, Rest/bits>>, _, _) when Opcode < 3, Opcode > 0 ->
	return_rest(Opcode, parse_rest(Len, MaskKey, Rest, 0));
parse_data(<<8:4, Opcode:4, 1:1, 127:7, Len:64, MaskKey:32, Rest/bits>>, _, _) when Opcode < 3, Opcode > 0 ->
	return_rest(Opcode, parse_rest(Len, MaskKey, Rest, 0));
parse_data(Data, MaskKey, UnmaskedLen) when UnmaskedLen > 0 ->
	parse_rest(byte_size(Data), MaskKey, Data, UnmaskedLen);
parse_data(_, _, _) ->
	error.

parse_rest(Len, MaskKey, Rest, UnmaskedLen) ->
	{Data, _, Eof} = split_rest(Rest, Len),
%%	<<Data:Len/binary, _/bits>> = Rest,
%%	Payload = unmask(Data, MaskKey, UnmaskedLen),
	Payload = unmask(Data, MaskKey, 0),
	PayloadLen = byte_size(Payload),
	case Eof of
		true ->
			{binary, Payload};
		false ->
			{more, MaskKey, Payload, PayloadLen + UnmaskedLen}
	end.

split_rest(Rest, Len) ->
	case byte_size(Rest) of
		Len ->
			{Rest, <<>>, true};
		RestLen when RestLen < Len ->
			{Rest, <<>>, false};
		_ ->
			<<Data:Len/binary, Rest2/bits>> = Rest,
			{Data, Rest2, true}
	end.

return_rest(Opcode, Data) ->
	case Data of
		{more, _, _, _} ->
			Data;
		{binary, Payload} ->
			case Opcode of
				1 ->
					{text, binary_to_list(Payload)};
				2 ->
					{binary, Payload}
			end
	end.

%%应答tcp握手
reply(Bin) ->
	SecKey = list_to_binary(lists:last(string:tokens(hd(lists:filter(fun(S) ->
		lists:prefix("Sec-WebSocket-Key:", S) end, string:tokens(binary_to_list(Bin), "\r\n"))), ": "))),
%%	上一语句拆分如下
%%	DataList = string:tokens(binary_to_list(Bin), "\r\n"),
%%	KeyList = lists:filter(fun(S) -> lists:prefix("Sec-WebSocket-Key:", S) end, DataList),
%%	SecKey = list_to_binary(lists:last(string:tokens(hd(KeyList), ": "))),

	Accept = base64:encode(crypto:hash(sha, <<SecKey/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)),
	[
		"HTTP/1.1 101 Switching Protocols\r\n",
		"connection: Upgrade\r\n",
		"upgrade: websocket\r\n",
		"sec-websocket-accept: ", Accept, "\r\n",
		"\r\n"
	].

%%由于Browser发过来的数据都是mask的,所以需要unmask
%%unmask(<<>>, _, Unmasked) ->
%%	Unmasked;
%%unmask(<<O:32, Rest/bits>>, MaskKey, Acc) ->
%%	T = O bxor MaskKey,
%%	unmask(Rest, MaskKey, <<Acc/binary, T:32>>);
%%unmask(<<O:24>>, MaskKey, Acc) ->
%%	<<MaskKey2:24, _:8>> = <<MaskKey:32>>,
%%	T = O bxor MaskKey2,
%%	<<Acc/binary, T:24>>;
%%unmask(<<O:16>>, MaskKey, Acc) ->
%%	<<MaskKey2:16, _:16>> = <<MaskKey:32>>,
%%	T = O bxor MaskKey2,
%%	<<Acc/binary, T:16>>;
%%unmask(<<O:8>>, MaskKey, Acc) ->
%%	<<MaskKey2:8, _:24>> = <<MaskKey:32>>,
%%	T = O bxor MaskKey2,
%%	<<Acc/binary, T:8>>.


unmask(Data, undefined, _) ->
	Data;
unmask(Data, MaskKey, 0) ->
	mask(Data, MaskKey, <<>>);
%% We unmask on the fly so we need to continue from the right mask byte.
unmask(Data, MaskKey, UnmaskedLen) ->
	Left = UnmaskedLen rem 4,
	Right = 4 - Left,
	MaskKey2 = (MaskKey bsl (Left * 8)) + (MaskKey bsr (Right * 8)),
	mask(Data, MaskKey2, <<>>).

mask(<<>>, _, Unmasked) ->
	Unmasked;
mask(<<O:32, Rest/bits>>, MaskKey, Acc) ->
	T = O bxor MaskKey,
	mask(Rest, MaskKey, <<Acc/binary, T:32>>);
mask(<<O:24>>, MaskKey, Acc) ->
	<<MaskKey2:24, _:8>> = <<MaskKey:32>>,
	T = O bxor MaskKey2,
	<<Acc/binary, T:24>>;
mask(<<O:16>>, MaskKey, Acc) ->
	<<MaskKey2:16, _:16>> = <<MaskKey:32>>,
	T = O bxor MaskKey2,
	<<Acc/binary, T:16>>;
mask(<<O:8>>, MaskKey, Acc) ->
	<<MaskKey2:8, _:24>> = <<MaskKey:32>>,
	T = O bxor MaskKey2,
	<<Acc/binary, T:8>>.