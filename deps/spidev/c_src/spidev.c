#include <wiringPiSPI.h>
#include <erl_nif.h>
#include <errno.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>

typedef struct {
  ErlNifEnv *env;
  int fd;
  unsigned long speed;
} state_t;

extern int errno;

static state_t state0;
static state_t state1;

static ERL_NIF_TERM setup_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	unsigned int channel;
	unsigned long speed;
	int error;

	if (argc != 2 || !enif_is_number(env, argv[0]) || !enif_is_number(env, argv[1])) {
		return enif_make_badarg(env);
	}
	if (!enif_get_uint(env, argv[0], &channel)) {
		return enif_make_badarg(env);
	}
	if (!enif_get_ulong(env, argv[1], &speed)) {
		return enif_make_badarg(env);
	}
	if (speed < 500000 || speed > 32000000) {
		return enif_make_badarg(env);
	}
	switch (channel) {
		case 0:
			if (state0.fd != 0) {
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "channel already opened", ERL_NIF_LATIN1));
			}
			else {
				state0.env = env;
				state0.fd = wiringPiSPISetup(channel, speed);
				if (state0.fd == 0) {
					error = errno;
					return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, error));
				}
				else {
					return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_int(env, channel));
				}
			}
			break;
		case 1:
			if (state1.fd != 0) {
				return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "channel already opened", ERL_NIF_LATIN1));
			}
			else {
				state1.env = env;
				state1.fd = wiringPiSPISetup(channel, speed);
				if (state1.fd == 0) {
					error = errno;
					return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, error));
				}
				else {
					return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_int(env, channel));
				}
			}
			break;
		default:
			return enif_make_badarg(env);
	}
}

static ERL_NIF_TERM close_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int channel;

	if (argc != 1 || !enif_is_number(env, argv[0])) {
    	return enif_make_badarg(env);
    }
	if (!enif_get_uint(env, argv[0], &channel)) {
		return enif_make_badarg(env);
	}
	switch (channel) {
		case 0:
			if (state0.fd != 0) {
				close(state0.fd);
				state0.fd = 0;
			}
			break;
		case 1:
			if (state1.fd != 0) {
				close(state1.fd);
				state1.fd = 0;
			}
			break;
		default:
			return enif_make_badarg(env);
	}
    
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM xfer_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int channel, length;
    int result, i, val;
    uint8_t *buffer;
    ERL_NIF_TERM cell, head, tail, list;

	if (argc < 2 || !enif_is_number(env, argv[0]) || !enif_is_list(env, argv[1])) {
    	return enif_make_badarg(env);
    }
    if (!enif_get_uint(env, argv[0], &channel) || channel < 0 || channel > 1) {
    	return enif_make_badarg(env);
    }
    if (!enif_get_list_length(env, argv[1], &length) || length == 0) {
    	return enif_make_badarg(env);
    }
    buffer = (uint8_t *) enif_alloc(sizeof(uint8_t) * length);
    list = argv[1];
	for (i = 0; enif_get_list_cell(env, list, &head, &tail); ++i, list = tail) {
		if (!enif_get_int(env, head, &val)) {
			return enif_make_badarg(env);
		}
		buffer[i] = val;
	}
	result = wiringPiSPIDataRW(channel, buffer, length);
	if (result == -1) {
		result = errno;
		enif_free(buffer);
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, result));
	}
	list = enif_make_list(env, 0);
	for (i = length - 1; i >= 0; --i) {
		cell = enif_make_uint(env, (unsigned int) buffer[i]);
		list = enif_make_list_cell(env, cell, list);
	}
	enif_free(buffer);

	return enif_make_tuple2(env, enif_make_atom(env, "ok"), list);
}

static ErlNifFunc spidev_NIFs[] = {
    {"xfer", 2, &xfer_2},
    {"setup", 2, &setup_2},
    {"close", 1, &close_1}
};

ERL_NIF_INIT(spidev, spidev_NIFs, NULL, NULL, NULL, NULL);
