CFLAGS=-c -Wall -fPIC -I/home/alex/programmi/eliot/lib/erlang/erts-5.9/include -I/opt/lib/erlang/lib/erl_interface-3.7.6/include
LDFLAGS= -shared -L/home/alex/programmi/eliot/lib/erlang/lib/erl_interface-3.7.6/lib

all: spidev

spidev: spidev.o
	gcc $(LDFLAGS) spidev.o -o spidev_drv.so

spidev.o:
	gcc $(CFLAGS) spidev.c

clean:
	rm -f *.o spidev.o
