%% @author vetal
%% @doc @todo Add description to cache_server_api.

-module(cache_server_api).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init_db/1, insert/3, lookup/2, delete/2, lookup_by_date/3]).

init_db(TabName) when is_atom(TabName) ->
	TabName = ets:new(TabName, [public, named_table]).

insert(TabName, Key, Value) when is_atom(TabName) ->
	true = ets:insert(TabName, {Key, {Value, cur_time()}}).

lookup(TabName, Key) when is_atom(TabName) ->
	case ets:lookup(TabName, Key) of
		[] -> undefined;
		%% TODO check ttl
		[{Key, {Value, Time}}] -> {ok, Value};
		Err -> {error, Err}
	end.

lookup_by_date(TabName, DateFrom, DateTo) ->
	SecFrom = calendar:datetime_to_gregorian_seconds(DateFrom),
	SecTo = calendar:datetime_to_gregorian_seconds(DateTo),
	F = fun({Key, {Value, RowSec}}, Acc) ->
		if (RowSec >= SecFrom andalso RowSec =< SecTo) ->
			[{Key, Value} | Acc];
			true -> Acc
		end
	end,
	ets:foldl(F, [], TabName).

delete(TabName, Key) when is_atom(TabName) ->
	true = ets:delete(TabName, Key).

%% ====================================================================
%% Internal functions
%% ====================================================================

cur_time() ->
	calendar:datetime_to_gregorian_seconds(calendar:local_time()).