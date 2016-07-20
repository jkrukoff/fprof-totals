-module(fprof_totals_cli).
-mode(compile).

-export([main/1]).

-spec main(any()) -> no_return().
main([File, Dest, Sort]) ->
    fprof_totals:analyse([
        {file, File},
        {dest, Dest},
        {sort, list_to_atom(Sort)}]),
    erlang:halt(0);
main([File, Dest]) ->
    fprof_totals:analyse(File, Dest),
    erlang:halt(0);
main(_) ->
    io:fwrite(
        "Usage: fproftotals <infile> <outfile> [sort:fun|cnt|acc|own]~n",
        []),
    erlang:halt(1).
