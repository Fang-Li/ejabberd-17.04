-module(mc_cursor).

-behaviour(gen_server).

-include("mongo_protocol.hrl").

-export([
	create/5,
        next/1, next/2,
	rest/1, rest/2,
	take/2, take/3,
	foldl/4, foldl/5,
	map/3,
	close/1
]).

-export([
	start_link/1
]).

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-record(state, {
	connection :: mc_worker:connection(),
	collection :: atom(),
	cursor :: integer(),
	batchsize :: integer(),
	batch :: [bson:document()],
	monitor :: reference
}).

-spec create(mc_worker:connection(), atom(), integer(), integer(), [bson:document()]) -> pid().
create(Connection, Collection, Cursor, BatchSize, Batch) ->
	{ok, Pid} = mc_cursor_sup:start_cursor([self(), Connection, Collection, Cursor, BatchSize, Batch]),
	Pid.

-spec next(pid()) -> {} | {bson:document()}.
next(Cursor) ->
        next(Cursor, cursor_default_timeout()).

-spec next(pid(), timeout()) -> {} | {bson:document()}.
next(Cursor, Timeout) ->
        try gen_server:call(Cursor, {next, Timeout}, Timeout) of
		Result -> Result
	catch
		exit:{noproc, _} -> {}
	end.

-spec rest(pid()) -> [bson:document()].
rest(Cursor) ->
        rest(Cursor, cursor_default_timeout()).

-spec rest(pid(), timeout()) -> [bson:document()].
rest(Cursor, Timeout) ->
	try gen_server:call(Cursor, {rest, infinity, Timeout}, Timeout) of
		Result -> Result
	catch
		exit:{noproc, _} -> []
	end.

-spec take(pid(), non_neg_integer()) -> [bson:document()].
take(Cursor, Limit) ->
        take(Cursor, Limit, cursor_default_timeout()).

-spec take(pid(), non_neg_integer(), timeout()) -> [bson:document()].
take(Cursor, Limit, Timeout) ->
	try gen_server:call(Cursor, {rest, Limit, Timeout}, Timeout) of
		Result -> Result
	catch
		exit:{noproc, _} -> []
	end.

cursor_default_timeout() ->
        application:get_env(mongodb, cursor_timeout, infinity).

-spec foldl(fun((bson:document(), term()) -> term()), term(), pid(), non_neg_integer()) -> term().
foldl(Fun, Acc, Cursor, Max) ->
        foldl(Fun, Acc, Cursor, Max, cursor_default_timeout()).

-spec foldl(fun((bson:document(), term()) -> term()), term(), pid(), non_neg_integer(), timeout()) -> term().
foldl(_Fun, Acc, _Cursor, 0, _Timeout) ->
	Acc;
foldl(Fun, Acc, Cursor, infinity, Timeout) ->
	lists:foldl(Fun, Acc, rest(Cursor, Timeout));
foldl(Fun, Acc, Cursor, Max, Timeout) ->
	case next(Cursor, Timeout) of
		{} -> Acc;
		{Doc} -> foldl(Fun, Fun(Doc, Acc), Cursor, Max - 1, Timeout)
	end.

-spec map(fun((bson:document()) -> term()), pid(), non_neg_integer()) -> [term()].
map(Fun, Cursor, Max) ->
	lists:reverse(foldl(fun(Doc, Acc) ->
		[Fun(Doc) | Acc]
	end, [], Cursor, Max)).

-spec close(pid()) -> ok.
close(Cursor) ->
	gen_server:cast(Cursor, halt).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).


%% @hidden
init([Owner, Connection, Collection, Cursor, BatchSize, Batch]) ->
	{ok, #state{
		connection = Connection,
		collection = Collection,
		cursor = Cursor,
		batchsize = BatchSize,
		batch = Batch,
		monitor = erlang:monitor(process, Owner)
	}}.

%% @hidden
handle_call({next, Timeout}, _From, State) ->
	case next_i(State, Timeout) of
		{Reply, #state{cursor = 0, batch = []} = UpdatedState} ->
			{stop, normal, Reply, UpdatedState};
		{Reply, UpdatedState} ->
			{reply, Reply, UpdatedState}
	end;
handle_call({rest, Limit, Timeout}, _From, State) ->
	case rest_i(State, Limit, Timeout) of
		{Reply, #state{cursor = 0} = UpdatedState} ->
			{stop, normal, Reply, UpdatedState};
		{Reply, UpdatedState} ->
			{reply, Reply, UpdatedState}
	end.

%% @hidden
handle_cast(halt, State) ->
  {stop, normal, State};
handle_cast(_, State) ->
	{noreply, State}.

%% @hidden
handle_info({'DOWN', Monitor, process, _, _}, #state{monitor = Monitor} = State) ->
	{stop, normal, State};
handle_info(_, State) ->
	{noreply, State}.

%% @hidden
terminate(_, #state{cursor = 0}) ->	ok;
terminate(_, State) ->
	gen_server:call(State#state.connection, #killcursor{cursorids = [State#state.cursor]}).

%% @hidden
code_change(_Old, State, _Extra) ->
	{ok, State}.

%% @private
next_i(#state{batch = [Doc | Rest]} = State, _Timeout) ->
	{{Doc}, State#state{batch = Rest}};
next_i(#state{batch = [], cursor = 0} = State, _Timeout) ->
	{{}, State};
next_i(#state{batch = []} = State, Timeout) ->
    Reply = gen_server:call(
              State#state.connection,
              #getmore{
                 collection = State#state.collection,
                 batchsize = State#state.batchsize,
                 cursorid = State#state.cursor
                },
              Timeout),
	Cursor = Reply#reply.cursorid,
	Batch = Reply#reply.documents,
        next_i(State#state{cursor = Cursor, batch = Batch}, Timeout).

%% @private
rest_i(State, infinity, Timeout) ->
	rest_i(State, -1, Timeout);
rest_i(State, Limit, Timeout) when is_integer(Limit) ->
	{Docs, UpdatedState} = rest_i(State, [], Limit, Timeout),
	{lists:reverse(Docs), UpdatedState}.

%% @private
rest_i(State, Acc, 0, _Timeout) ->
	{Acc, State};
rest_i(State, Acc, Limit, Timeout) ->
	case next_i(State, Timeout) of
		{{}, UpdatedState} -> {Acc, UpdatedState};
		{{Doc}, UpdatedState} ->
			rest_i(UpdatedState, [Doc | Acc], Limit - 1, Timeout)
	end.