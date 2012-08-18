/*
   Copyright (c) 2012 Alessandro Sivieri <sivieri@elet.polimi.it>

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

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ei.h>

#include "common.h"
#include "io.h"

void write_packet(char* buf, int size, FILE* fd) {
    uint8_t hd[4];
    
    hd[0] = (size >> 24) & 0xff;
    hd[1] = (size >> 16) & 0xff;
    hd[2] = (size >> 8) & 0xff;
    hd[3] = size & 0xff;
    fwrite(hd, 1, 4, fd);
    fwrite(buf, 1, size, fd);
    fflush(fd);
}

static size_t read_bytes(unsigned char* buf, size_t max, FILE* fd) {
    size_t n;

    n = fread(buf, 1, max, fd);
    if ((n == 0) && !feof(fd)) {
        exit(ERR_READ);
    }

    return n;
}

void read_packet(unsigned char* buf, size_t max, FILE* fd) {
    size_t n, size;
    uint8_t hd[4];

    n = read_bytes(hd, 4, fd);
    if (n == 0 && feof(fd)) {
        exit(EXIT_SUCCESS);
    }
    if (n != 4) {
        exit(ERR_READ_HEADER);
    }
    size = (hd[0] << 24) + (hd[1] << 16) + (hd[2] << 8) + hd[3];
    if (size > max) {
        exit(ERR_PACKET_SIZE);
    }
    n = read_bytes(buf, size, fd);
    if (n != size) {
        exit(ERR_READ);
    }
}
