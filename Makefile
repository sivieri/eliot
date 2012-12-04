all: deploy

lib:
	(cd c_src/gpio;$(MAKE))
	(cd c_src/rssi;$(MAKE))
	cp c_src/gpio/eliot-gpio priv/
	cp c_src/rssi/eliot-rssi priv/

deploy: lib
	(mkdir -p ebin;cd src;$(MAKE) deploy)

simulation: lib
	(mkdir -p simbin;cd src;$(MAKE) simulation)

clean:
	rm -f ebin/*
	rm -f simbin/*
	(cd c_src/gpio; $(MAKE) clean)
	(cd c_src/rssi; $(MAKE) clean)
