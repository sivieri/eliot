all: deploy

lib:
	(cd c_src;$(MAKE))
	cp c_src/eliot_udp.so priv/

deploy: lib
	(cd src;$(MAKE) deploy)

simulation: lib
	(cd src;$(MAKE) simulation)

clean:
	rm -f ebin/*
	rm -f simbin/*
	(cd c_src; $(MAKE) clean)
