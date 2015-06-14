%% @author vetal
%% @doc @todo Add description to cache_server.


-module(cache_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

start() ->
	application:start(cache_server),
	case get_env(tcp_api_enabled, false) of
		false -> do_nothing;
		true ->
			application:start(ranch),
			Port = get_env(tcp_api_port, 5555),
			{ok, _} = ranch:start_listener(tcp_api, 100, ranch_tcp, [{port, Port}], cache_server_tcp, []);
		_ -> exit(wrong_config)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

get_env(Key, Def) ->
	case application:get_env(cache_server, Key) of
		{ok, Val} -> Val;
		undefined -> Def
	end.