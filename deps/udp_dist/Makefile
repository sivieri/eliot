all: deploy

lib:
	(mkdir -p priv;cd c_src;$(MAKE))
	cp c_src/eliot_udp.so priv/

deploy:
	(mkdir -p ebin;cd src;$(MAKE) deploy)

simulation: lib
	(mkdir -p simbin;cd src;$(MAKE) simulation)

clean:
	rm -f ebin/*
	rm -f simbin/*
	(cd c_src; $(MAKE) clean)
