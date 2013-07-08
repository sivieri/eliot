#include <wiringPiI2C.h>
#include <erl_nif.h>
#include <errno.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>

typedef struct {
  ErlNifEnv *env;
  int fd;
  int device;
} status_t;

extern int errno;

static status_t status;

static ERL_NIF_TERM setup_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	unsigned int d;
	int error;

	if (argc != 1 || !enif_is_number(env, argv[0])) {
		return enif_make_badarg(env);
	}
	if (!enif_get_uint(env, argv[0], &d)) {
		return enif_make_badarg(env);
	}
    if (status.fd != 0) {
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "Close previous devices", ERL_NIF_LATIN1));
    }
	status.env = env;
    status.device = d;
	status.fd = wiringPiI2CSetup(d);
	if (status.fd == -1) {
		error = errno;
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, error));
	}

	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM close_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	if (argc != 0) {
    	return enif_make_badarg(env);
    }
	close(status.fd);

	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM read_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	int result;

	if (argc != 0) {
		return enif_make_badarg(env);
	}
	result = wiringPiI2CRead(status.fd);
	if (result == -1) {
		result = errno;
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, result));
	}

	return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_int(env, result));
}

static ERL_NIF_TERM read_reg8_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	int result, reg;

	if (argc != 1 || !enif_is_number(env, argv[0])) {
		return enif_make_badarg(env);
	}
	if (!enif_get_int(env, argv[0], &reg)) {
		return enif_make_badarg(env);
	}
	result = wiringPiI2CReadReg8(status.fd, reg);
	if (result == -1) {
		result = errno;
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, result));
	}

	return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_int(env, result));
}

static ERL_NIF_TERM read_reg16_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	int result, reg;

	if (argc != 1 || !enif_is_number(env, argv[0])) {
		return enif_make_badarg(env);
	}
	if (!enif_get_int(env, argv[0], &reg)) {
		return enif_make_badarg(env);
	}
	result = wiringPiI2CReadReg16(status.fd, reg);
	if (result == -1) {
		result = errno;
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, result));
	}

	return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_int(env, result));
}

static ERL_NIF_TERM write_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	int data, result;

	if (argc != 1 || !enif_is_number(env, argv[0])) {
		return enif_make_badarg(env);
	}
	if (!enif_get_int(env, argv[0], &data)) {
		return enif_make_badarg(env);
	}
	result = wiringPiI2CWrite(status.fd, data);
	if (result == -1) {
		result = errno;
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, result));
	}

	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM write_reg8_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	int data, reg, result;

	if (argc != 2 || !enif_is_number(env, argv[0]) || !enif_is_number(env, argv[1])) {
		return enif_make_badarg(env);
	}
	if (!enif_get_int(env, argv[0], &reg)) {
		return enif_make_badarg(env);
	}
	if (!enif_get_int(env, argv[1], &data)) {
		return enif_make_badarg(env);
	}
	result = wiringPiI2CWriteReg8(status.fd, reg, data);
	if (result == -1) {
		result = errno;
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, result));
	}

	return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM write_reg16_2(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
	int data, reg, result;

	if (argc != 2 || !enif_is_number(env, argv[0]) || !enif_is_number(env, argv[1])) {
		return enif_make_badarg(env);
	}
	if (!enif_get_int(env, argv[0], &reg)) {
		return enif_make_badarg(env);
	}
	if (!enif_get_int(env, argv[1], &data)) {
		return enif_make_badarg(env);
	}
	result = wiringPiI2CWriteReg16(status.fd, reg, data);
	if (result == -1) {
		result = errno;
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, result));
	}

	return enif_make_atom(env, "ok");
}

static ErlNifFunc i2cdev_NIFs[] = {
    {"setup", 1, &setup_1},
    {"close", 0, &close_0},
    {"read", 0, &read_0},
    {"read_reg8", 1, &read_reg8_1},
    {"read_reg16", 1, &read_reg16_1},
    {"write", 1, &write_1},
	{"write_reg8", 2, &write_reg8_2},
	{"write_reg16", 2, &write_reg16_2},
};

ERL_NIF_INIT(i2cdev, i2cdev_NIFs, NULL, NULL, NULL, NULL);
