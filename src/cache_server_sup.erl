
-module(cache_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	case get_env(tcp_api_enabled, false) of
		false -> do_nothing;
		true ->
			TcpPort = get_env(tcp_api_port, 5555),
			{ok, _} = ranch:start_listener(tcp_api, 100, ranch_tcp, [{port, TcpPort}], cache_server_tcp, []);
		_ -> exit(wrong_config)
	end,
	case get_env(http_api_enabled, false) of
		false -> do_nothing;
		true ->
			HttpPort = get_env(http_api_port, 8080),
			Dispatch = cowboy_router:compile([
											  {'_', [{"/", cache_server_http, []},
													 {"/api/cache_server", cache_server_http, []}]}
											 ]),
			{ok, _} = cowboy:start_http(my_http_listener, 100, [{port, HttpPort}],
										[{env, [{dispatch, Dispatch}]}]
									   );
		_ -> exit(wrong_config)
	end,
	
	{ok, { {one_for_one, 5, 10}, [
								  ?CHILD(cache_server_srv, worker)
								 ]} }.


get_env(Key, Def) ->
	case application:get_env(cache_server, Key) of
		{ok, Val} -> Val;
		undefined -> Def
	end.