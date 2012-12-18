all: deploy

lib:
	(cd c_src/spi;$(MAKE))
	(cd c_src/gpio;$(MAKE))
	(cd c_src/rssi;$(MAKE))
	cp c_src/spi/spidev_drv.so priv/
	cp c_src/gpio/eliot-gpio priv/
	cp c_src/rssi/eliot-rssi priv/

deploy:
	(mkdir -p ebin;cd src;$(MAKE) deploy)

simulation:
	(mkdir -p simbin;cd src;$(MAKE) simulation)

clean:
	rm -f ebin/*
	rm -f simbin/*
	(cd c_src/spi; $(MAKE) clean)
	(cd c_src/gpio; $(MAKE) clean)
	(cd c_src/rssi; $(MAKE) clean)
	rm -f priv/spidev_drv.so
	rm -f priv/eliot-gpio
	rm -f priv/eliot-rssi
