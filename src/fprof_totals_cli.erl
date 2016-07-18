-module(fprof_totals_cli).
-mode(compile).

-export([main/1]).

main([File, Dest]) ->
    fprof_totals:analyse(File, Dest),
    erlang:halt(0);
main(_) ->
    io:format("Usage: fproftotals <infile> <outfile>~n", []),
    erlang:halt(1).
