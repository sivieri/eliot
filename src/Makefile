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

ELIOT_DIST = eliot_api.erl eliot_app.erl eliot_dispatcher.erl eliot_forwarder.erl eliot_gpio.erl eliot_rssi.erl eliot_simulator.erl eliot_sup.erl eliot.erl utils.erl demo_recv.erl demo_send.erl clocks.erl
ELIOT_PARSER = eliot_parser.erl
ELIOT_ALGS = ctp_fwd.erl ctp_link.erl ctp_routing.erl eliot_ctp.erl eliot_oppflooder_event.erl eliot_oppflooder.erl eliot_trickle.erl

all: deploy

parser:
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $(ELIOT_PARSER)

parser_sim:
	$(ERLC) $(ERLC_FLAGS) -o $(SIMBIN_DIR) $(ELIOT_PARSER)

deploy: parser
	cp *.app ../ebin
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $(ELIOT_DIST) $(ELIOT_ALGS)

simulation: parser_sim
	cp *.app ../simbin
	$(ERLC) $(ERLC_SIM_FLAGS) -o $(SIMBIN_DIR) $(ELIOT_DIST)
	$(ERLC) $(ERLC_SIM_PARSER_FLAGS) -o $(SIMBIN_DIR) $(ELIOT_ALGS)
