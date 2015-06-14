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
	delete(TabName, Key),
	true = ets:insert(TabName, {{Key, cur_time()}, Value}).

lookup(TabName, Key) when is_atom(TabName) ->
	case rows_by_key(TabName, Key) of
		[] -> undefined;
		Res ->
			InternalKey = hd(Res),
			case ets:lookup(TabName, InternalKey) of
				[] -> undefined;
				[{InternalKey, Value}] -> {ok, Value};
				Err -> {error, Err}
			end
	end.

lookup_by_date(TabName, DateFrom, DateTo) ->
	SecFrom = calendar:datetime_to_gregorian_seconds(DateFrom),
	SecTo = calendar:datetime_to_gregorian_seconds(DateTo),
	F = fun({{Key, RowSec}, Val}, Acc) ->
		if (RowSec >= SecFrom andalso RowSec =< SecTo) ->
			[{Key, Val} | Acc];
			true -> Acc
		end
	end,
	ets:foldl(F, [], TabName).

delete(TabName, Key) when is_atom(TabName) ->
	Keys = rows_by_key(TabName, Key),
	[true = ets:delete(TabName, K) || K <- Keys],
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


cur_time() ->
	calendar:datetime_to_gregorian_seconds(calendar:local_time()).

rows_by_key(TabName, Key) ->
	rows_by_key(TabName, Key, ets:first(TabName), []).

rows_by_key(_TabName, _Key, '$end_of_table', Acc) ->
	lists:reverse(Acc);

rows_by_key(TabName, Key, {Key, _} = CurKey, Acc) ->
	rows_by_key(TabName, Key, ets:next(TabName, CurKey), [CurKey | Acc]);

rows_by_key(TabName, Key, CurKey, Acc) ->
	rows_by_key(TabName, Key, ets:next(TabName, CurKey), Acc).