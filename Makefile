.PHONY: doc
include bu.mk

compile:
	$(verbose) $(REBAR) compile

tests:
	$(verbose) $(REBAR) eunit

doc:
	$(verbose) $(REBAR) as doc edoc

elixir:
	$(verbose) $(REBAR) elixir generate_mix
	$(verbose) $(REBAR) elixir generate_lib

dist: compile tests elixir doc

distclean:
	$(verbose) rm -rf _build rebar.lock mix.lock test/eunit deps

