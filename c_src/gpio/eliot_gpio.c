/*
   Copyright (c) 2012 Alessandro Sivieri <alessandro.sivieri@gmail.com>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License version 3 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; see the file COPYING.LIB.  If not, write to
   the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.
*/

/**
 * Possible input values:
 * - {in, integer()}
 * - {out, integer()}
 * - {get, integer()}
 * - {set, integer(), low|high}
 * 
 * Possible output values:
 * - {error, "Error msg"}
 * - {ok, low|high}
 * - ok
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ei.h>

#include "gpio.h"
#include "io.h"
#include "common.h"

#define BUFSIZE 128
#define ATOMSIZE 10
#define MAXPIN 14
#define DEVNAME "/dev/gpio"

typedef struct {
    ei_x_buff x;
    char errmsg[BUFSIZE];
} state_t;

static void make_error(state_t *state, const char* text) {
    ei_x_free(&state->x);
    ei_x_new_with_version(&state->x);
    ei_x_encode_tuple_header(&state->x, 2);
    ei_x_encode_atom(&state->x, "error");
    ei_x_encode_string(&state->x, text);
}

int process_request(int fd, unsigned char* buf, int index, char* msg) {
    int arity, result;
    long pin;
    char cmd[ATOMSIZE], value[ATOMSIZE];
    
    if (ei_decode_tuple_header((char*) buf, &index, &arity)) {
        msg = "Expecting a tuple";
        return -1;
    }
    if (arity == 2 || arity == 3) {
        if (ei_decode_atom((char*) buf, &index, cmd)) {
            msg = "First element must be an atom";
            return -1;
        }
        if (ei_decode_long((char*) buf, &index, &pin)) {
            msg = "Second element must be a pin number";
            return -1;
        }
        if (pin > MAXPIN) {
            msg = "Wrong pin number";
            return -1;
        }
        if (arity == 3 && strcmp(cmd, "set") == 0) {
            if (ei_decode_atom((char*) buf, &index, value)) {
                msg = "Set argument must be the atom low|high";
                return -1;
            }
            if (strcmp(value, "low") == 0) {
                result = pin_clear(fd, (int) pin);
                if (result < 0) {
                    msg = "Ioctl error";
                    return -1;
                }
                else return 0;
            }
            else if (strcmp(value, "high") == 0) {
                result = pin_set(fd, (int) pin);
                if (result < 0) {
                    msg = "Ioctl error";
                    return -1;
                }
                else return 0;
            }
            else {
                msg = "Set argument must be the atom low|high";
                return -1;
            }
        }
        else if (arity == 2 && strcmp(cmd, "get") == 0) {
            result = pin_get(fd, (int) pin);
            if (result < 0) {
                msg = "Ioctl error";
                return -1;
            }
            else return result ? 2 : 1;
        }
        else if (arity == 2 && strcmp(cmd, "in") == 0) {
            result = pin_dirin(fd, (int) pin);
            if (result < 0) {
                msg = "Ioctl error";
                return -1;
            }
            else return 0;
        }
        else if (arity == 2 && strcmp(cmd, "out") == 0) {
            result = pin_dirout(fd, (int) pin);
            if (result < 0) {
                msg = "Ioctl error";
                return -1;
            }
            else return 0;
        }
        else {
            msg = "Command must be atom set|get|in|out";
            return -1;
        }
    }
    else {
        msg = "Expecting a 2/3 dimension tuple";
        return -1;
    }
}

static void process_data(int fd, unsigned char* buf) {
    state_t st;
    int index = 0, version = 0, result;
    char msg[BUFSIZE];
    
    ei_x_new_with_version(&st.x);
    if (ei_decode_version((char*) buf, &index, &version)) {
        make_error(&st, "Data encoding version mismatch");
    }
    else {
        result = process_request(fd, buf, index, msg);
        if (result < 0) {
            make_error(&st, msg);
        }
        else if (result > 0) {
            ei_x_encode_tuple_header(&st.x, 2);
            ei_x_encode_atom(&st.x, "ok");
            ei_x_encode_atom(&st.x, result == 1 ? "low" : "high");
        }
        else {
            ei_x_encode_atom(&st.x, "ok");
        }
    }
    write_packet(st.x.buff, st.x.buffsz, stdout);
    ei_x_free(&st.x);
}

int main(int argc, char** argv) {
    static unsigned char buf[BUFSIZE];
    int fd = 0;
    state_t st;

#ifdef __mips__
    fd = open(DEVNAME, O_RDWR);
#endif
    if (fd < 0) {
        ei_x_new_with_version(&st.x);
        make_error(&st, "Unable to open the device");
        write_packet(st.x.buff, st.x.buffsz, stdout);
        ei_x_free(&st.x);
        exit(ERR_OPEN);
    }
    for (;;) {
        read_packet(buf, sizeof(buf) - 1, stdin);
        buf[sizeof(buf) - 1] = 0;
        process_data(fd, buf);
    }
}
