PROJECT = tetrerl

DEPS = cowboy
TEST_DEPS = meck

dep_cowboy = git://github.com/extend/cowboy.git 0.10.0
dep_meck = git://github.com/eproxus/meck.git 0.8

.PHONY: release clean-release local

local:
	erl -sname tetrerl -config rel/sys.config -args_file rel/vm.args \
		-pa $PWD ebin deps/*/ebin -s tetrerl

release: clean-release
	relx -o rel/$(PROJECT)

clean-release:
	rm -rf rel/$(PROJECT)

include erlang.mk