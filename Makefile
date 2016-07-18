PROJECT = fprof_totals
PROJECT_DESCRIPTION = Calculate aggregates from fprof totals.
PROJECT_VERSION = 0.0.1
BUILD_DEPS = elvis_mk
DEP_PLUGINS = elvis_mk
ESCRIPT_NAME = fprof_totals_cli
ESCRIPT_FILE = fproftotals

dep_elvis_mk = git https://github.com/inaka/elvis.mk.git 1.0.0

include erlang.mk

check:: xref elvis

docs:: edoc
