%%%-------------------------------------------------------------------
%%% @author John Krukoff
%%% @copyright (C) 2016, John Krukoff
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fprof_totals_tests).

%-ifdef(TEST).

-import(fprof_totals, [analyse/2, analyse/1, aggregate/1]).
-include_lib("eunit/include/eunit.hrl").

-define(FPROF_FORMAT, "%% Analysis results:
{  analysis_options,
 [{callers, true},
  {sort, acc},
  {totals, true},
  {details, true}]}.

%                                               CNT       ACC       OWN        
[{ totals,                                     3658128,12559.758,49580.422}].  %%%

{~tp,{},[]}.
").

-define(SAMPLE_TOTAL, [
    {{cowboy_websocket,handler_loop,4},             1,12536.730,    0.000},
    {{piqi_tools,port_receive_loop,1},             24,12534.816,    0.000},
    {{cowboy_websocket,handler_loop,4},             1,12532.809,    0.000},
    {{cowboy_websocket,handler_loop,4},             1,12531.964,    0.000},
    {{gen_server,loop,6},                          15,12529.465,    0.000},
    {{gen_server,loop,6},                          27,12529.155,    0.000},
    {{gen_server,loop,6},                          24,12528.985,    0.000}]).

-define(SAMPLE_EXPECTED, [
    {{piqi_tools,port_receive_loop,1},             24,12534.816,    0.000},
    {{gen_server,loop,6},                          66,37587.605,    0.000},
    {{cowboy_websocket,handler_loop,4},             3,37601.503,    0.000}]).

-define(SINGLE_TOTAL, [
    {{gen_server,loop,6},                          24,12528.985,    0.000}]).

-define(SINGLE_EXPECTED, ?SINGLE_TOTAL).

-define(SAME_TOTAL, [
    {{gen_server,loop,6},                          27,12529.155,    0.000},
    {{gen_server,loop,6},                          24,12528.985,    0.000}]).

-define(SAME_EXPECTED, [
    {{gen_server,loop,6},                          51,25058.140,    0.000}]).

-define(DIFFERING_TOTAL, [
    {{cowboy_websocket,handler_loop,4},             1,12531.964,    0.000},
    {{gen_server,loop,6},                          15,12529.465,    0.000}]).

-define(DIFFERING_EXPECTED, ?DIFFERING_TOTAL).

compare_unordered(Expected, Aggregated) ->
    lists:all(fun(V) ->
                  lists:any(fun(E) -> V =:= E end, Expected)
              end, Aggregated).

compare_aggregate(Expected, Total) ->
    Aggregated = aggregate(Total),
    compare_unordered(Expected, Aggregated).

aggregate_test_() ->
    {inparallel,
     [?_assert(compare_aggregate(?SAMPLE_EXPECTED, ?SAMPLE_TOTAL)),
      ?_assert(compare_aggregate(?SINGLE_EXPECTED, ?SINGLE_TOTAL)),
      ?_assert(compare_aggregate(?SAME_EXPECTED, ?SAME_TOTAL)),
      ?_assert(compare_aggregate(?DIFFERING_EXPECTED, ?DIFFERING_TOTAL))]}.

temp_file() ->
    lib:nonl(?cmd("mktemp")).

compare_analyse({Expected, _Total}, {_Filename, Destination}) ->
    {ok, [Aggregated|_]} = file:consult(Destination),
    compare_unordered(Expected, Aggregated).

assert_analyse1({Expected, Total}, {Filename, Destination}) ->
    analyse([{file, Filename}, {dest, Destination}]),
    ?_assert(compare_analyse({Expected, Total}, {Filename, Destination})).

assert_analyse2({Expected, Total}, {Filename, Destination}) ->
    analyse(Filename, Destination),
    ?_assert(compare_analyse({Expected, Total}, {Filename, Destination})).

analyse_test_() ->
    Setup = fun({_Expected, Total}) ->
        Formatted = io_lib:format(?FPROF_FORMAT, [Total]),
        Filename = temp_file(),
        ok = file:write_file(Filename, Formatted),
        Destination = temp_file(),
        {Filename, Destination}
    end,
    Teardown = fun({_Expected, _Total}, {Filename, Destination}) ->
        ok = file:delete(Filename),
        ok = file:delete(Destination)
    end,
    {inparallel,
     {foreachx,
      Setup,
      Teardown,
      [{{?SAMPLE_EXPECTED, ?SAMPLE_TOTAL}, fun assert_analyse1/2},
       {{?SINGLE_EXPECTED, ?SINGLE_TOTAL}, fun assert_analyse1/2},
       {{?SAME_EXPECTED, ?SAME_TOTAL}, fun assert_analyse1/2},
       {{?DIFFERING_EXPECTED, ?DIFFERING_TOTAL}, fun assert_analyse1/2},
       {{?SAMPLE_EXPECTED, ?SAMPLE_TOTAL}, fun assert_analyse2/2},
       {{?SINGLE_EXPECTED, ?SINGLE_TOTAL}, fun assert_analyse2/2},
       {{?SAME_EXPECTED, ?SAME_TOTAL}, fun assert_analyse2/2},
       {{?DIFFERING_EXPECTED, ?DIFFERING_TOTAL}, fun assert_analyse2/2}]}}.

%-endif.
