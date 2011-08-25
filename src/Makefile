include ../support/include.mk

APPLICATION=wsn
DOC_OPTS={dir,\"../doc\"}

all: $(EBIN_FILES_NO_DOCS)

edoc:
	mkdir -p ../doc
	$(ERL) -noshell -pa ../ebin \
            -eval "edoc:application($(APPLICATION), \".\", [$(DOC_OPTS)])" \
            -s init stop

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
