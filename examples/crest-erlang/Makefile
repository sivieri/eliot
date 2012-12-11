all: deploy

deploy:
	(mkdir -p ebin;cd src;$(MAKE) deploy)

simulation:
	(mkdir -p simbin;cd src;$(MAKE) simulation)

clean:
	rm -f ebin/*
	rm -f simbin/*
