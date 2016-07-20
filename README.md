fprof Totals
============

This is a simple OTP library for aggregating the totals section of an
[fprof](http://erlang.org/doc/man/fprof.html) analysis. Why didn't I just use
[cprof](http://erlang.org/doc/man/cprof.html) in the first place, you might
ask? Excellent question!

![No good answer](https://media.giphy.com/media/dXICCcws9oxxK/giphy.gif)

Development
-----------

This is an [erlang.mk](https://erlang.mk/) based project, so all commands are
part of the Makefile.

Help: `make help`
Compile: `make`
Run Tests: `make check`
Build docs: `make docs`
Build escript: `make escript`

Library Usage
-------------

For library usage, generated documentation is available
[in the repository](doc/index.html).

Escript
-------

The library can also be used as the escript `fproftotals`, built with `make
escript`. This will provide a simple CLI for converting fprof analysis files
into an aggregated version. An optional 3rd parameter allows for choosing the
sort criteria.

`Usage: fproftotals <infile> <outfile> [sort:fun|cnt|acc|own]`
