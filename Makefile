PROJECT = tetrerl

DEPS = cowboy

dep_cowboy = https://github.com/extend/cowboy.git 0.8.5

.PHONY: release clean-release

release: clean-release all projects
    relx -o rel/$(PROJECT)

clean-release: clean-projects
    rm -rf rel/$(PROJECT)

include erlang.mk