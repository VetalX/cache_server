%% @author vetal
%% @doc @todo Add description to cache_server_http.


-module(cache_server_http).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
	{Method, Req2} = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req2),
	{ok, Req3} = parse_req(Method, HasBody, Req2),
	{ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.


%% ====================================================================
%% Internal functions
%% ====================================================================

parse_req(<<"POST">>, true, Req) ->
	{ok, PostVals, Req2} = cowboy_req:body_qs(Req),
 	try 
		[{BinJson, true}] = PostVals,
		Json = jsx:decode(BinJson),
		Action = proplists:get_value(<<"action">>, Json),
		RespBody = case parse_command(Action, Json) of
			{ok, OkRes} -> OkRes;
			{error, ErrorRes} -> ErrorRes;
			UndefRes -> term_to_binary(UndefRes)
		end,
		
		cowboy_req:reply(200, [
							   {<<"content-type">>, <<"text/plain; charset=utf-8">>}
							  ], RespBody, Req2)
	catch
		_:_ -> cowboy_req:reply(500, [], <<"Wrong JSON format">>, Req2)
	end;

parse_req(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);

parse_req(<<"GET">>, _, Req) ->
	cowboy_req:reply(200, [], <<"Try to POST Json object here /api/cache_server">>, Req);

parse_req(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

parse_command(<<"insert">>, Json) ->
	Key = proplists:get_value(<<"key">>, Json),
	Value = proplists:get_value(<<"value">>, Json),
	cache_server_srv:insert(Key, Value);

parse_command(<<"lookup">>, Json) ->
	Key = proplists:get_value(<<"key">>, Json),
	cache_server_srv:lookup(Key);

parse_command(<<"delete">>, Json) ->
	Key = proplists:get_value(<<"key">>, Json),
	cache_server_srv:delete(Key);

parse_command(_, _) ->
	<<"wrong_comand">>.