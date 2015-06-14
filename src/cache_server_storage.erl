%% @author vetal
%% @doc @todo Add description to cache_server_storage.

-module(cache_server_storage).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init_db/1, insert/3, lookup/3, delete/2, lookup_by_date/4, gc/2]).

init_db(TabName) when is_atom(TabName) ->
	TabName = ets:new(TabName, [public, named_table]).

insert(TabName, Key, Value) when is_atom(TabName) ->
	true = ets:insert(TabName, {Key, {Value, cur_time()}}).

lookup(TabName, Key, Ttl) when is_atom(TabName) ->
	case ets:lookup(TabName, Key) of
		[] -> undefined;
		[{Key, {Value, Time}}] -> 
			Diff = cur_time() - Time,
			if Diff < Ttl ->
				   {ok, Value}; 
			   true -> undefined
			end;
		Err -> {error, Err}
	end.

lookup_by_date(TabName, DateFrom, DateTo, Ttl) ->
	SecFrom = datetime_to_sec(DateFrom),
	SecTo = datetime_to_sec(DateTo),
	CurTime = cur_time(),
	F = fun({Key, {Value, RowSec}}, Acc) ->
		if (RowSec >= SecFrom andalso RowSec =< SecTo andalso CurTime - RowSec < Ttl) ->
			[{Key, Value} | Acc];
			true -> Acc
		end
	end,
	ets:foldl(F, [], TabName).

delete(TabName, Key) when is_atom(TabName) ->
	true = ets:delete(TabName, Key).

gc(TabName, Ttl) ->
	CurTime = cur_time(),
	gc(TabName, Ttl, CurTime, ets:first(TabName)).

%% ====================================================================
%% Internal functions
%% ====================================================================

gc(_TabName, _Ttl, _CurTime, '$end_of_table') ->
	ok;

gc(TabName, Ttl, CurTime, Key) ->
	[{Key, {_Value, RowTime}}] = ets:lookup(TabName, Key),
	NextKey = ets:next(TabName, Key),
	if(CurTime - RowTime > Ttl) 
		-> true = ets:delete(TabName, Key);
	  true -> do_nothing
	end,
	gc(TabName, Ttl, CurTime, NextKey).


cur_time() ->
	calendar:datetime_to_gregorian_seconds(calendar:local_time()).

datetime_to_sec(DateTime) ->
	calendar:datetime_to_gregorian_seconds(DateTime).