HAS_ELIXIR=1

include bu.mk

release: dist lint tag
	$(verbose) $(REBAR) hex publish

