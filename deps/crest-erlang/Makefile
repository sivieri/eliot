.PHONY: all download deps deploy simulation clean

all: deploy

download:
	./rebar get-deps

deps:
	$(MAKE) -C deps/ibrowse all
	$(MAKE) -C deps/log4erl all
	$(MAKE) -C deps/mochiweb all

deploy:
	(mkdir -p ebin;cd src;$(MAKE) deploy)

simulation: deps
	(mkdir -p simbin;cd src;$(MAKE) simulation)

clean:
	rm -f ebin/*
	rm -f simbin/*
	$(MAKE) -C deps/ibrowse clean
	$(MAKE) -C deps/log4erl clean
	$(MAKE) -C deps/mochiweb clean
