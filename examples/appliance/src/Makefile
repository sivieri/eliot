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

APP_SRC = appliance_app.erl appliance_handler.erl appliance_model.erl appliance_sup.erl appliance_task.erl appliance.erl appliances.erl

all: deploy

deploy:
	cp *.app ../ebin
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $(APP_SRC)

simulation:
	cp *.app ../simbin
	$(ERLC) $(ERLC_SIM_PARSER_FLAGS) -o $(SIMBIN_DIR) $(APP_SRC)
