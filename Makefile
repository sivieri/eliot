all:
	mkdir -p ebin
	(cd src;$(MAKE))

edoc:
	(cd src;$(MAKE) edoc)

clean:
	(cd src;$(MAKE) clean)

typer:
	typer -I src src/*.erl
