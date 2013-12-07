REBAR=$(shell which rebar)

ifeq ($(REBAR),)
$(error "Make sure you have rebar in your PATH.")
endif

all:
	$(REBAR) compile

clean:
	rm -rf $(CURDIR)/ebin
