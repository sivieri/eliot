#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <erl_nif.h>
#include <fcntl.h>
#include <linux/spi/spidev.h>
#include <linux/types.h>
#include <sys/ioctl.h>
#include <unistd.h>

typedef struct {
  ErlNifEnv *env;
  int fd;
  uint8_t mode;
  uint8_t bits_per_word;
  uint8_t max_speed_hz;
} state_t;

static state_t state;

static ERL_NIF_TERM open_1(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[])
{
    char *device;
    unsigned int length;
    uint8_t tmp8;
    uint32_t tmp32;
    
    state.env = env;
    state.fd = -1;
    state.mode = 0;
    state.bits_per_word = 0;
    state.max_speed_hz = 0;
    if (argc != 1 || !enif_is_list(env, argv[0]))
        return enif_make_badarg(env);
    if (!enif_get_list_length(env, argv[0], &length))
        return enif_make_badarg(env);
    device = (char *) enif_alloc(length + 1);
    enif_get_string(env, argv[0], device, length + 1, ERL_NIF_LATIN1);
    if ((state.fd = open(device, O_RDWR, 0)) == -1) {
        return enif_make_badarg(env);
    }
    if (ioctl(state.fd, SPI_IOC_RD_MODE, &tmp8) == -1) {
        close(state.fd);
        enif_free(device);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "read mode", ERL_NIF_LATIN1));
    }
    state.mode = tmp8;
    if (ioctl(state.fd, SPI_IOC_RD_BITS_PER_WORD, &tmp8) == -1) {
        close(state.fd);
        enif_free(device);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "read bits per word", ERL_NIF_LATIN1));
    }
    state.bits_per_word = tmp8;
    if (ioctl(state.fd, SPI_IOC_RD_MAX_SPEED_HZ, &tmp32) == -1) {
        close(state.fd);
        enif_free(device);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "read max speed hz", ERL_NIF_LATIN1));
    }
    state.max_speed_hz = tmp32;
    enif_free(device);
    
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM close_0(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[])
{
    close(state.fd);
    state.fd = -1;
    state.mode = 0;
    state.bits_per_word = 0;
    state.max_speed_hz = 0;
    
    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM xfer2_1(ErlNifEnv *env, int argc,
                                     const ERL_NIF_TERM argv[])
{
    int status, val, i;
    struct spi_ioc_transfer xfer;
    uint8_t *txbuf, *rxbuf;
    unsigned int length;
    ERL_NIF_TERM cell, head, tail, res, list;
    
    if (state.fd == -1)
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "device closed", ERL_NIF_LATIN1));
    if (argc != 1 || !enif_is_list(env, argv[0]))
        return enif_make_badarg(env);
    if (!enif_get_list_length(env, argv[0], &length))
        return enif_make_badarg(env);
    txbuf = (uint8_t *) enif_alloc(sizeof(uint8_t) * length);
    rxbuf = (uint8_t *) enif_alloc(sizeof(uint8_t) * length);
    list = argv[0];
    for (i = 0; enif_get_list_cell(env, list, &head, &tail); ++i, list = tail) {
        if (!enif_get_int(env, head, &val)) {
            return enif_make_badarg(env);
        }
        txbuf[i] = val;
    }
    xfer.tx_buf = (unsigned long) txbuf;
    xfer.rx_buf = (unsigned long) rxbuf;
    xfer.len = length;
    status = ioctl(state.fd, SPI_IOC_MESSAGE(1), &xfer);
    if (status < 0) {
        enif_free(txbuf);
        enif_free(rxbuf);
        return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_string(env, "spi ioc message", ERL_NIF_LATIN1));
    }
    list = enif_make_list(env, 0);
    for (i = length - 1; i >= 0; --i) {
        cell = enif_make_uint(env, (unsigned int) rxbuf[i]);
        list = enif_make_list_cell(env, cell, list);
    }
    res = enif_make_tuple2(env, enif_make_atom(env, "ok"), list);
    enif_free(txbuf);
    enif_free(rxbuf);
    
    return res;
}

static ErlNifFunc spidev_NIFs[] = {
    {"xfer2", 1, &xfer2_1},
    {"open", 1, &open_1},
    {"close", 0, &close_0}
};

ERL_NIF_INIT(spidev, spidev_NIFs, NULL, NULL, NULL, NULL);
