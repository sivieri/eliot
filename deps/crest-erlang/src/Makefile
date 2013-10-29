INCLUDE_DIRS := ../include $(wildcard ../deps/*/include)
EBIN_DIRS := ../ebin $(wildcard ../deps/*/ebin)
EBIN_DIR := ../ebin
ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %) +debug_info -I ../../xmerl/include
ERLC := erlc

CREST_SRC = crest_app.erl crest_sup.erl crest.erl crest_deps.erl crest_json.erl crest_local.erl crest_manager.erl crest_operations.erl crest_peer.erl crest_spawn.erl crest_spawn_sup.erl crest_utils.erl crest_web.erl demo.erl demo_text_mining.erl destructure_json.erl gen_server2.erl original.erl uuid.erl demo_temp_fun.erl demo_temp.erl  demo_temp_sens.erl crest_html.erl

all: deploy

deploy:
	cp *.app ../ebin
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $(CREST_SRC)
