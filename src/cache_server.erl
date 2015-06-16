%% @author vetal
%% @doc @todo Add description to cache_server.


-module(cache_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).

start() ->
	application:start(ranch),
	application:start(crypto),
	application:start(cowlib),
	application:start(cowboy),
	application:start(jsx),
	application:start(cache_server).

%% ====================================================================
%% Internal functions
%% ====================================================================
