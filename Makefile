.PHONY: compile
compile:
	./rebar compile

.PHONY: clean
clean:
	./rebar clean

TESTS=""
.PHONY: test
test:
ifeq ($(TESTS), "")
	./rebar eunit
else
	./rebar eunit suite=$(TESTS)
endif

.PHONY: doc
doc:
	./rebar doc

shell: compile
	erl -pa .eunit -pa ebin -s modlib_reloader
