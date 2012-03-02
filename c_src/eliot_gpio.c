#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <linux/gpio_dev.h>
#include <linux/ioctl.h>
#include <ei.h>

#define ERR_READ 10
#define ERR_READ_HEADER 11
#define ERR_PACKET_SIZE 12
#define BUFSIZE 128

typedef struct {
    ei_x_buff x;
    char errmsg[256];
} state_t;

static void write_packet(char* buf, int size, FILE* fd) {
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

static void read_packet(unsigned char* buf, size_t max, FILE* fd) {
    size_t n, size;
    uint8_t hd[4];
    
    n = read_bytes(hd, 4, fd);
    if (n == 0 && feof(hd)) {
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

static void make_error(state_t *state, const char* text) {
    ei_x_free(&state->x);
    ei_x_new_with_version(&state->x);
    ei_x_encode_tuple_header(&state->x, 2);
    ei_x_encode_atom(&state->x, "error");
    ei_x_encode_string(&st->x, text);
}

static void process_data(unsigned char* buf) {
    
}

int main(int argv, char** argv) {
    static unsigned char buf[BUFSIZE];
    
    for (;;) {
        read_packed(buf, sizeof(buf) - 1, stdin);
        buf[sizeof(buf) - 1] = 0;
        process_data(buf);
    }
}
