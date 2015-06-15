%% @author vetal
%% @doc @todo Add description to cache_server_srv.


-module(cache_server_srv).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, insert/2, delete/1, lookup/1, lookup_by_date/2]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(Key, Value) ->
	gen_server:call(?MODULE, {api_insert, {Key, Value}}).

delete(Key) ->
	gen_server:call(?MODULE, {api_delete, Key}).

lookup(Key) ->
	gen_server:call(?MODULE, {api_lookup, Key}).

lookup_by_date(DateFrom, DateTo) ->
	gen_server:call(?MODULE, {api_lookup_by_date, {DateFrom, DateTo}}).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {
				tab_name,
				ttl,
				ttl_check_period
			   }).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	Ttl = get_env(ttl, 3600),
	TtlCheckPeriod = get_env(ttl_check_period, 60),
	TabName = get_env(tab_name, cache),
	TabName = cache_server_storage:init_db(TabName),
	check_ttl(TtlCheckPeriod),
	{ok, #state{ttl = Ttl, tab_name = TabName, ttl_check_period = TtlCheckPeriod}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================

handle_call({api_insert, {Key, Value}}, _From, #state{tab_name = TabName} = State) ->
	Reply = try
				cache_server_storage:insert(TabName, Key, Value)
			catch
				_:_ -> {error, <<"wrong_command">>}
			end,
	{reply, Reply, State};

handle_call({api_delete, Key}, _From, #state{tab_name = TabName} = State) ->
	Reply = try
				cache_server_storage:delete(TabName, Key)
			catch
				_:_ -> {error, <<"wrong_command">>}
			end,
	{reply, Reply, State};

handle_call({api_lookup, Key}, _From, #state{tab_name = TabName, ttl = Ttl} = State) ->
	Reply = try
				cache_server_storage:lookup(TabName, Key, Ttl)
			catch
				_:_ -> {error, <<"wrong_command">>}
			end,
	{reply, Reply, State};

handle_call({api_lookup_by_date, {DateFrom, DateTo}}, _From, #state{tab_name = TabName, ttl = Ttl} = State) ->
	Reply = try
				cache_server_storage:lookup_by_date(TabName, DateFrom, DateTo, Ttl)
			catch
				_:_ -> {error, <<"wrong_command">>}
			end,
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(check_ttl, #state{ttl_check_period = TtlCheckPeriod, tab_name = TabName, ttl = Ttl} = State) ->
	proc_lib:spawn(fun() -> cache_server_storage:gc(TabName, Ttl) end),
	check_ttl(TtlCheckPeriod),
	{noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

check_ttl(TtlCheckPeriod) ->
	erlang:send_after(TtlCheckPeriod * 1000, self(), check_ttl).

get_env(Key, Def) ->
	case application:get_env(Key) of
		{ok, Val} -> Val;
		undefined -> Def
	end.