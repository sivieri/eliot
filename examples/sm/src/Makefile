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

SM_SRC = sm_algorithm.erl sm_app.erl sm_current.erl sm_sup.erl sm_task.erl sm.erl data.erl

all: deploy

deploy:
	cp *.app ../ebin
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $(SM_SRC)

simulation:
	cp *.app ../simbin
	$(ERLC) $(ERLC_SIM_PARSER_FLAGS) -o $(SIMBIN_DIR) $(SM_SRC)
