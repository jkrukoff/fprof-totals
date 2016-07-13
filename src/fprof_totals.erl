%%%-------------------------------------------------------------------
%%% @author John Krukoff
%%% @copyright (C) 2016, John Krukoff
%%% @doc
%%%
%%% @end
%%% Created : 2016-06-29 15:42:23.524413
%%%-------------------------------------------------------------------
-module(fprof_totals).

-behaviour(gen_server).

%% API
-export([analyse/1, analyse/2, aggregate/1]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

analyse(File, Dest) ->
	analyse([{file, File}, {dest, Dest}]).

analyse(Options) ->
	File = proplists:get_value(file, Options),
	Dest = proplists:get_value(dest, Options),
	{ok, Pid} = gen_server:start_link(?MODULE, [], []),
	ok = gen_server:call(Pid, {analyse, File, Dest}, infinity),
	gen_server:cast(Pid, stop),
	ok.

aggregate(Totals) ->
	Aggregated = lists:foldl(fun({Fun, Cnt, Acc, Own}, Aggregates) ->
			{CCnt, CAcc, COwn} = maps:get(Fun, Aggregates, {0.0, 0.0, 0.0}),
			Aggregates#{Fun => {CCnt + Cnt, CAcc + Acc, COwn + Own}}
		end,
		#{},
		Totals),
	[{Fun, Cnt, Acc, Own} || {Fun, {Cnt, Acc, Own}} <- maps:to_list(Aggregated)].
	% maps:fold(fun(Fun, {Cnt, Acc, Own}, Acc) ->
	% 		[{Fun, Cnt, Acc, Own} | Acc]
	% 	end,
	% 	[],
	% 	Aggregated).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%	{ok, State, Timeout} |
%%	ignore |
%%	{stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%	{reply, Reply, State} |
%%	{reply, Reply, State, Timeout} |
%%	{noreply, State} |
%%	{noreply, State, Timeout} |
%%	{stop, Reason, Reply, State} |
%%	{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({analyse, File, Dest}, _From, State) ->
	{ok, Terms} = file:consult(File),
	[{analysis_options, _}, [{totals, _, _, _}], {Totals, _, _} | _] = Terms,
	Aggregated = pmap(Totals, erlang:system_info(logical_processors_available)),
	ok = file:write_file(Dest, io_lib:format("~tp.~n", [Aggregated])),
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%	{noreply, State, Timeout} |
%%	{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
	{stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%	{noreply, State, Timeout} |
%%	{stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
split(Totals, N) ->
	split(Totals, length(Totals) div N, N, []).

split(Remainder, _Size, 1, []) ->
	[Remainder];
split(Remainder, _Size, 1, Acc) ->
	[Remainder | Acc];
split(Remainder, Size, N, Acc) when N > 1 ->
	{H, Remainder2} = lists:split(Size, Remainder),
	split(Remainder2, Size, N-1, [H | Acc]).

pmap(Totals, undefined) ->
	aggregate(Totals);
pmap(Totals, 1) ->
	aggregate(Totals);
pmap(Totals, N) when N > 1 ->
	Split = split(Totals, N),
	Aggregates = rpc:pmap({fprof_totals, aggregate}, [], Split),
	aggregate(lists:flatten(Aggregates)).
