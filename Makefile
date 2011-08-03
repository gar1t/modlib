compile: deps
	./rebar compile

quick:
	./rebar compile skip_deps=true

deps:
	./rebar get-deps

refresh-deps:
	./rebar delete-deps
	./rebar get-deps

clean:
	./rebar clean

.PHONY: test

DEPS=$(notdir $(wildcard deps/*))

TESTS=""
test:
ifeq ($(TESTS), "")
	./rebar -j1 eunit skip_deps=true
else
	./rebar -j1 eunit suite=$(TESTS) skip_deps=true
endif

.PHONY: doc
doc:
	./rebar doc
