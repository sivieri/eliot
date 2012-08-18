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
 * - {scan, string()}
 * 
 * Possible output values:
 * - {error, "Error msg"}
 * - {ok, [{string(), int()}]}
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ei.h>
#ifdef __mips__
#include <iwinfo.h>
#endif

#include "io.h"
#include "common.h"

#define BUFSIZE 128
#define BUFSIZE2 1024
#define MACLENGTH 19
#define ATOMSIZE 10
#define DEVSIZE 16
#define ENTRYLENGTH 8

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

static void get_mac(char* buf, int index, char* res) {
    uint8_t tmp[6];
    int i;
    
    for (i = 0; i < 6; ++i) {
        tmp[i] = buf[index + i] & 0xff;
    }
    snprintf(res, MACLENGTH - 1, "%02x:%02x:%02x:%02x:%02x:%02x", tmp[0], tmp[1], tmp[2], tmp[3], tmp[4], tmp[5]);
}

static int process_request(unsigned char* buf, int index, char* msg, char* buf2) {
    int arity, result;
    char cmd[ATOMSIZE], value[DEVSIZE];
    const struct iwinfo_ops *ops;
    
    if (ei_decode_tuple_header((char*) buf, &index, &arity)) {
        msg = "Expecting a tuple";
        return -1;
    }
    if (arity == 2) {
        if (ei_decode_atom((char*) buf, &index, cmd)) {
            msg = "First element must be an atom";
            return -1;
        }
        if (strcmp(cmd, "scan") == 0) {
            if (ei_decode_string((char*) buf, &index, value)) {
                msg = "Second element must be a device name";
                return -1;
            }
#ifdef __mips__
            ops = iwinfo_backend(value);
            if (ops->assoclist(value, buf2, &result) != 0) {
                msg = "Unable to get associates list";
                return -1;
            }
#else
            strcpy(buf2, "test");
            result = 4;
#endif
            return result;
        }
        else {
            msg = "Command must be the atom scan";
            return -1;
        }
    }
    else {
        msg = "Expecting a 2-dimension tuple";
        return -1;
    }
}

static void process_data(unsigned char* buf) {
    state_t st;
    int index = 0, version = 0, result, i;
    uint8_t rssi;
    char msg[BUFSIZE], buf2[BUFSIZE2], mac[MACLENGTH];
    
    ei_x_new_with_version(&st.x);
    if (ei_decode_version((char*) buf, &index, &version)) {
        make_error(&st, "Data encoding version mismatch");
    }
    else {
        result = process_request(buf, index, msg, buf2);
        if (result < 0) {
            make_error(&st, msg);
        }
        else {
            ei_x_encode_tuple_header(&st.x, 2);
            ei_x_encode_atom(&st.x, "ok");
            ei_x_encode_list_header(&st.x, result / ENTRYLENGTH);
            for (i = 0; i < result / ENTRYLENGTH; ++i) {
                get_mac(buf2, i * ENTRYLENGTH, mac);
                rssi = buf2[i * ENTRYLENGTH + 6] & 0xff;
                ei_x_encode_tuple_header(&st.x, 2);
                ei_x_encode_string(&st.x, mac);
                ei_x_encode_long(&st.x, rssi);
            }
            ei_x_encode_empty_list(&st.x);
        }
    }
    write_packet(st.x.buff, st.x.buffsz, stdout);
    ei_x_free(&st.x);
}

int main(int argc, char** argv) {
    static unsigned char buf[BUFSIZE];

    for (;;) {
        read_packet(buf, sizeof(buf) - 1, stdin);
        buf[sizeof(buf) - 1] = 0;
        process_data(buf);
    }
}
