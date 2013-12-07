REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Make sure you have rebar in your PATH.")
endif

all: deps compile

compile:
	$(REBAR) skip_deps=true compile

deps:
	$(REBAR) get-deps
	$(REBAR) compile

doc:
	$(REBAR) skip_deps=true doc

test:
	$(REBAR) skip_deps=true eunit

clean:
	rm -rf $(CURDIR)/ebin
	rm -rf $(CURDIR)/doc
	$(REBAR) skip_deps=true clean

distclean: clean
	rm -rf $(CURDIR)/deps
