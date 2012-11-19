INCLUDE_DIRS := ../include $(wildcard ../deps/*/include)
EBIN_DIRS := ../ebin $(wildcard ../deps/*/ebin)
SIMBIN_DIRS := ../simbin $(wildcard ../deps/*/simbin)
EBIN_DIR := ../ebin
SIMBIN_DIR := ../simbin
ERLC_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %) +debug_info
ERLC_SIM_BASE_FLAGS := -W $(INCLUDE_DIRS:../%=-I ../%) $(SIMBIN_DIRS:%=-pa %) +debug_info
ERLC_SIM_FLAGS := $(ERLC_SIM_BASE_FLAGS) -Dsimulation
ERLC_SIM_PARSER_FLAGS := $(ERLC_SIM_FLAGS) +'{parse_transform, eliot_parser}'
ERLC := erlc

CCBR_SRC = oppflooder_app.erl oppflooder_handler.erl oppflooder_sup.erl oppflooder_task.erl oppflooder.erl

all: deploy

deploy:
	cp *.app ../ebin
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $(CCBR_SRC)

simulation:
	cp *.app ../simbin
	$(ERLC) $(ERLC_SIM_PARSER_FLAGS) -o $(SIMBIN_DIR) $(CCBR_SRC)
