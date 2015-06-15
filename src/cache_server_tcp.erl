%% @author vetal
%% @doc @todo Add description to cache_server_tcp.

-module(cache_server_tcp).
-behaviour(ranch_protocol).


%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/4, init/4]).

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
			case parse_data(Data) of
				<<>> -> do_nothing;
				Response -> 
					Msg = case process_command(Response) of
						{ok, OkRes} -> OkRes;
						{error, ErrorRes} -> ErrorRes;
						UndefRes -> term_to_binary(UndefRes)
					end,
					Tail = <<"\r\n">>,
					Transport:send(Socket, <<Msg/binary, Tail/binary>>)
			end,
			loop(Socket, Transport, RecvTimeout);
		_ ->
			ok = Transport:close(Socket)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

process_command(<<"get ", Key/binary>>) ->
	cache_server_srv:lookup(Key);

process_command(<<"del ", Key/binary>>) ->
	cache_server_srv:delete(Key);

process_command(<<"set ", Rest/binary>>) ->
	[Key, Value] = binary:split(Rest, <<" ">>),
	cache_server_srv:insert(Key, Value);

process_command(_) ->
	{error, <<"wrong command">>}.

parse_data(B) when is_binary(B) ->
	binary:part(B, {0, byte_size(B)-2}).

get_env(Key, Def) ->
	case application:get_env(cache_server, Key) of
		{ok, Val} -> Val;
		undefined -> Def
	end.