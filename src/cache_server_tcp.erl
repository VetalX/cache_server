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
					Msg = 
						try
							case process_command(Response) of
								{ok, OkRes} -> jsx:encode([<<"ok">>, OkRes]);
								{error, ErrorRes} -> jsx:encode([<<"error">>, ErrorRes]);
								UndefRes -> term_to_binary(UndefRes)
							end
						catch 
							_:_ -> jsx:encode([<<"error">>, <<"wrong comand">>])
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

process_command(<<"get_by_date ", Params/binary>>) ->
	[DateFrom, TimeFrom, DateTo, TimeTo] = split(Params, <<" ">>),
	DateTimeFrom = binary_to_datetime(DateFrom, TimeFrom),
	DateTimeTo = binary_to_datetime(DateTo, TimeTo),
	cache_server_srv:lookup_by_date(DateTimeFrom, DateTimeTo);

process_command(<<"del ", Key/binary>>) ->
	cache_server_srv:delete(Key);

process_command(<<"set ", Rest/binary>>) ->
	[Key, Value] = binary:split(Rest, <<" ">>),
	cache_server_srv:insert(Key, Value);

process_command(_) ->
	{error, <<"wrong command">>}.

parse_data(B) when is_binary(B) ->
	binary:part(B, {0, byte_size(B)-2}).

binary_to_datetime(Date, Time) ->
	[Year, Month, Day] = split(Date, <<"/">>),
	[Hour, Minute, Sec] = split(Time, <<":">>),
	{{bi(Year), bi(Month), bi(Day)},{bi(Hour), bi(Minute), bi(Sec)}}.

bi(B) -> binary_to_integer(B).

split(Bin, Split) when is_binary(Split) ->
    split(Bin, byte_size(Split), Split, <<>>, []).

split(Bin, SplitSize, Split, Acc1, Acc2) ->
    case Bin of
        <<Split:SplitSize/binary, Rest/binary>> ->
            split(Rest, SplitSize, Split, <<>>, [Acc1 | Acc2]);
        <<B:1/binary, Rest/binary>> ->
            split(Rest, SplitSize, Split, <<Acc1/binary, B:1/binary>>, Acc2);
        <<>> ->
            lists:reverse([Acc1 | Acc2])
    end.

get_env(Key, Def) ->
	case application:get_env(cache_server, Key) of
		{ok, Val} -> Val;
		undefined -> Def
	end.