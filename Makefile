PROJECT = tempfile

DEPS = bucs
dep_bucs = git https://github.com/botsunit/bucs.git master

DOC_DEPS = edown
dep_edown = git https://github.com/botsunit/edown.git master

include erlang.mk

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {top_level_readme, {"./README.md", "https://github.com/botsunit/tempfile"}}

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include

