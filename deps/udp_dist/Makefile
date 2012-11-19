all: deploy

lib:
	(cd c_src;$(MAKE))

deploy: lib
	(cd src;$(MAKE) deploy)

simulation: lib
	(cd src;$(MAKE) simulation)

clean:
	rm -f ebin/*
	rm -f simbin/*
	(cd c_src; $(MAKE) clean)
