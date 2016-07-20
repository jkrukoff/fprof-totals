%%%-------------------------------------------------------------------
%%% @author John Krukoff
%%% @copyright (C) 2016, John Krukoff
%%% @version 1.0.0
%%% @doc Library for aggregating fprof totals by function.
%%%-------------------------------------------------------------------
-module(fprof_totals).

-behaviour(gen_server).

%% API
-export([analyse/1,
    analyse/2,
    aggregate/1,
    sort/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).
-type profiled_function() :: {tuple(), non_neg_integer(), float(), float()}.
-type sort() :: 'fun' | cnt | acc | own.

%%%===================================================================
%%% API
%%%===================================================================

%% @equiv fun analyse/1
-spec analyse(File::string(), Dest::string()) -> ok.
analyse(File, Dest) ->
    analyse([{file, File}, {dest, Dest}]).

%% @doc
%% Given an fprof analysis file containing total information, aggregate the
%% totals section by function call and output the result to a new file.
%% @end
-spec analyse(Options::[{file | dest, string()} | {sort, sort()}]) -> ok.
analyse(Options) ->
    File = proplists:get_value(file, Options),
    Dest = proplists:get_value(dest, Options),
    Sort = proplists:get_value(sort, Options, acc),
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    ok = gen_server:call(Pid, {analyse, File, Dest, Sort}, infinity),
    gen_server:cast(Pid, stop),
    ok.

%% @doc
%% Given a list of fprof total tuples, return an aggregated list of function
%% call times and counts.
%% @end
-spec aggregate(Totals::[profiled_function()]) -> [profiled_function()].
aggregate(Totals) ->
    Aggregated = lists:foldl(fun({Fun, Cnt, Acc, Own}, Aggregates) ->
            {CCnt, CAcc, COwn} = maps:get(Fun, Aggregates, {0, 0.0, 0.0}),
            Aggregates#{Fun => {CCnt + Cnt, CAcc + Acc, COwn + Own}}
        end,
        #{},
        Totals),
    maps:fold(fun(Fun, {Cnt, Acc, Own}, Aggregates) ->
            [{Fun, Cnt, Acc, Own} | Aggregates]
        end,
        [],
        Aggregated).

%% @doc
%% Given a list of fprof total tuples, return a sorted list of function
%% call times and counts by the given index.
%% @end
-spec sort(Aggregated::[profiled_function()], Sort::sort()) ->
    [profiled_function()].
sort(Aggregated, 'fun') ->
    lists:keysort(1, Aggregated);
sort(Aggregated, Sort) ->
    N = case Sort of
        cnt -> 2;
        acc -> 3;
        own -> 4
    end,
    Sorted = lists:keysort(N, Aggregated),
    lists:reverse(Sorted).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call({analyse, File, Dest, Sort}, _From, State) ->
    {ok, Terms} = file:consult(File),
    [{analysis_options, _}, [{totals, _, _, _}], {Totals, _, _} | _] = Terms,
    Aggregated = pmap(Totals, erlang:system_info(logical_processors_available)),
    Sorted = sort(Aggregated, Sort),
    ok = write_file(Dest, Sorted),
    % ok = file:write_file(Dest, io_lib:format("~tp.~n", [Sorted])),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec split(Totals::[any()], N::integer()) -> [[any()]].
split(Totals, N) ->
    split(Totals, length(Totals) div N, N, []).

-spec split(
    Remainder::[any()],
    Size::integer(),
    N::integer(),
    Acc::[[any()]]) ->
    [[any()]].
split(Remainder, _Size, 1, []) ->
    [Remainder];
split(Remainder, _Size, 1, Acc) ->
    [Remainder | Acc];
split(Remainder, Size, N, Acc) when N > 1 ->
    {H, Remainder2} = lists:split(Size, Remainder),
    split(Remainder2, Size, N-1, [H | Acc]).

-spec pmap(Totals::[profiled_function()], N::integer()) ->
    [profiled_function()].
pmap(Totals, undefined) ->
    aggregate(Totals);
pmap(Totals, 1) ->
    aggregate(Totals);
pmap(Totals, N) when N > 1 ->
    Split = split(Totals, N),
    Aggregates = rpc:pmap({fprof_totals, aggregate}, [], Split),
    aggregate(lists:flatten(Aggregates)).

-spec write_file(Dest::string(), Aggregates::[profiled_function()]) -> ok.
write_file(Dest, Aggregates) ->
    {ok, Output} = file:open(Dest, [write, delayed_write]),
    ok = io:fwrite(
        Output, "% ~-38s~10s~14s~14s  ~n", ["FUN", "CNT", "ACC", "OWN"]),
    ok = io:fwrite(Output, "[~n", []),
    ok = write_aggregates(Output, Aggregates),
    ok = io:fwrite(Output, "].~n", []),
    ok = file:close(Output),
    ok.

-spec write_aggregates(
    Output::file:io_device(),
    Aggregates::[profiled_function()]) -> ok.
write_aggregates(_Output, []) ->
    ok;
write_aggregates(Output, [{Fun, Cnt, Acc, Own}]) ->
    ok = io:fwrite(Output, " {~w, ~b, ~.3f, ~.3f}~n", [Fun, Cnt, Acc, Own]),
    write_aggregates(Output, []);
write_aggregates(Output, [{Fun, Cnt, Acc, Own} | Aggregates]) ->
    ok = io:fwrite(Output, " {~w, ~b, ~.3f, ~.3f},~n", [Fun, Cnt, Acc, Own]),
    write_aggregates(Output, Aggregates).
