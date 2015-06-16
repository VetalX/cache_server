-module(cache_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
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
    cache_server_sup:start_link().

stop(_State) ->
    ok.

get_env(Key, Def) ->
    case application:get_env(cache_server, Key) of
        {ok, Val} -> Val;
        undefined -> Def
    end.