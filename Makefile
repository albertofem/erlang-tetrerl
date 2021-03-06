PROJECT = tetrerl

DEPS = cowboy eredis lager sync jsx eredis
TEST_DEPS = meck

dep_cowboy = git://github.com/extend/cowboy.git 0.10.0
dep_meck = git://github.com/eproxus/meck.git 0.8
dep_sync = git://github.com/shian/sync.git master
dep_lager = git://github.com/basho/lager.git 2.0.3
dep_eredis = git://github.com/wooga/eredis.git master
dep_jsx = git://github.com/talentdeficit/jsx.git v2.0.4

COMPILE_FIRST = tetrerl_game

.PHONY: run

run: all
	erl -sname tetrerl -config rel/sys.config -args_file rel/vm.args \
		-pa $PWD ebin deps/*/ebin -s tetrerl -s sync

include erlang.mk