%% @author vetal
%% @doc @todo Add description to cache_server_tcp.

-module(cache_server_tcp).
-behaviour(ranch_protocol).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4]).
-export([init/4]).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
	{ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
	RecvTimeout = get_env(tcp_api_recv_timeout, 3600) * 1000,
	ok = ranch:accept_ack(Ref),
	loop(Socket, Transport, RecvTimeout).

loop(Socket, Transport, RecvTimeout) ->
	case Transport:recv(Socket, 0, RecvTimeout) of
		{ok, Data} ->
			Res = gen_server:call(cache_server_srv, {tcp_command, Data}),
			Transport:send(Socket, <<Res/binary, <<"\r\n">>/binary>>),
			loop(Socket, Transport, RecvTimeout);
		_ ->
			ok = Transport:close(Socket)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_env(Key, Def) ->
	case application:get_env(cache_server, Key) of
		{ok, Val} -> Val;
		undefined -> Def
	end.