#include <erl_nif.h>

static ERL_NIF_TERM rssi_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    
}

static ErlNifFunc nif_funcs[] = {
    {"rssi", 1, rssi_nif}
};

ERL_NIF_INIT(eliot_rssi, nif_funcs, NULL, NULL, NULL, NULL)
