all: deploy

deploy:
	ELIOT_MODE=deploy ./rebar compile

simulation:
	ELIOT_MODE=simulation ./rebar compile

clean:
	ELIOT_MODE=deploy ./rebar clean
	ELIOT_MODE=simulation ./rebar clean

doc:
	./rebar doc
