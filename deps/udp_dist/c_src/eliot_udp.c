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
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <erl_driver.h>

#ifdef DEBUG
#define FPRINTF(...) fprintf(__VA_ARGS__)
#else
#define FPRINTF(...)
#endif

#define PORT 4369
#define BUF 1472
#define MINIBUF 10
#define DIST_MAGIC_RECV_TAG 131
#define RESEND_MAX_N 5
#define RESEND_MIN 100
#define EXP_BACKOFF 4
#define BEACON_TIMING 15
#define BEACON_TIMEOUT 60

#define DATA_MSG 'D'
#define DATA_MSG_ACK_REQUIRED 'R'
#define TICK_MSG 'T'
#define ACK_MSG 'A'
#define BEACON_MSG 'B'

typedef enum state
{
    INIT,
    LISTEN,
    ACCEPT,
    SEND,
    RECEIVE,
    CONNECT,
    HANDSHAKED,
    BEACONING,
    CLEANING
} state_t;

typedef struct driver_data
{
    ErlDrvPort port;
    struct sockaddr_in peer;
    int clientSock;
    state_t curstate;
    unsigned char creation;
    unsigned int sent;
    unsigned int received;
    unsigned long latest_beacon;
    uint32_t msg_number;
    struct driver_data* next;
} driver_data_t;

typedef struct ip_data
{
    struct sockaddr_in peer;
    struct ip_data* next;
} ip_data_t;

typedef struct int_data
{
    uint32_t number;
    struct int_data *next;
} int_data_t;

typedef struct ack_data
{
     driver_data_t* res;
     struct sockaddr_in peer;
     char buf[BUF];
     int len;
     uint32_t msg;
     int resend;
     unsigned long last;
     ErlDrvTermData caller;
    struct ack_data* next;
} ack_data_t;

typedef struct thread
{
    ErlDrvTid erl_thread;
    driver_data_t *owner;
} thread_t;

/*
 * Driver functions
 */
static ErlDrvData drv_start(ErlDrvPort port, char* command);
static void drv_stop(ErlDrvData handle);
static void drv_ready_input(ErlDrvData handle, ErlDrvEvent event);
static void drv_output(ErlDrvData handle, char* buf, ErlDrvSizeT len);
static void drv_finish();
static ErlDrvSSizeT drv_control(ErlDrvData handle, unsigned int cmd, char* buf, ErlDrvSizeT size, char** res,
        ErlDrvSizeT res_size);
static void drv_timeout(ErlDrvData handle);
/*
 * Helper functions
 */
void free_entry(driver_data_t* element);
void do_recv(driver_data_t* res);
int do_send(driver_data_t* res, char* buf, int len, int reliable);
int do_send2(driver_data_t* res, char* buf, int len, int reliable);
void do_send_ack(int socket, uint32_t msg, struct sockaddr_in* client);
void do_resend(ack_data_t* ack);
void *do_beacon(void *data);
int check_expired(unsigned long now, unsigned long last, int resend);
void print_ports();
void print_acks();
void do_clean(driver_data_t *res);
unsigned long get_current_time();
unsigned long get_secs();
void do_send_upstairs(ack_data_t *ack, driver_data_t *res);
static void put_packet_length(char *b, int len);
static int report_control_error(char **buffer, int buff_len, 
                char *error_message);

static ErlDrvEntry udp_driver_entry = {
        NULL,
        drv_start,
        drv_stop,
        drv_output,
        drv_ready_input,
        NULL,
        "eliot_udp",
        drv_finish,
        NULL,
        drv_control,
        drv_timeout,
        NULL,
        NULL,
        NULL,
        NULL,
        NULL,
        ERL_DRV_EXTENDED_MARKER,
        ERL_DRV_EXTENDED_MAJOR_VERSION,
        ERL_DRV_EXTENDED_MINOR_VERSION,
        ERL_DRV_FLAG_SOFT_BUSY,
        NULL,
        NULL,
        NULL
};

static driver_data_t* head;
static ip_data_t* peers;
static ack_data_t* acks;
static int counter;
static int sock;
static int sockBool;
static struct sockaddr_in sa;
static ErlDrvMutex *mutex;
static ErlDrvMutex *ack_mutex;
static ErlDrvMutex *beacon_mutex;
static thread_t beacon_thread;
static int beaconBool;

/*
 * Driver functions
 */

DRIVER_INIT(eliot_udp) {
    head = NULL;
    peers = NULL;
    acks = NULL;
    counter = 0;
    mutex = erl_drv_mutex_create("UDP");
    ack_mutex = erl_drv_mutex_create("UDP_acks");
    beacon_mutex = erl_drv_mutex_create("UDP_beacon");
    memset(&sa, 0, sizeof(struct sockaddr_in));
    sa.sin_family = AF_INET;
    sa.sin_port = htons(PORT);
    sa.sin_addr.s_addr = htonl(INADDR_ANY);
    sock = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock < 0) {
        FPRINTF(stderr, "DEBUG: Unable to create the socket\n");
        return NULL;
    }
    if (bind(sock, (struct sockaddr*)&sa, sizeof(struct sockaddr_in)) < -1) {
        FPRINTF(stderr, "DEBUG: Unable to bind the socket\n");
        return NULL;
    }
    sockBool = 0;
    beaconBool = 1;
    
    FPRINTF(stderr, "DEBUG: UDP driver loaded\n");

    return &udp_driver_entry;
}

static ErlDrvData drv_start(ErlDrvPort port, char* command) {
    int res2, broadcastPermission = 1;
    driver_data_t* res = (driver_data_t*)driver_alloc(sizeof(driver_data_t));
    memset(&res->peer, 0, sizeof(struct sockaddr_in));
    res->port = port;
    res->clientSock = socket(AF_INET, SOCK_DGRAM, 0);
    if (res->clientSock < 0) {
        FPRINTF(stderr, "DEBUG: Unable to create a client socket\n");
        return (ErlDrvData) NULL;
    }
    if (!sockBool) {
        res2 = setsockopt(res->clientSock, SOL_SOCKET, SO_BROADCAST, (void *) &broadcastPermission, sizeof(broadcastPermission));
        if (res2 < 0) fprintf(stderr,"DEBUG: Unable to set broadcast: %d\n", errno);
        driver_select(port, (ErlDrvEvent)sock, ERL_DRV_READ, 1);
        driver_set_timer(port, RESEND_MIN);
        erl_drv_thread_create("beacon", &beacon_thread.erl_thread, do_beacon, &res->clientSock, NULL);
        beacon_thread.owner = res;
        sockBool = 1;
    }
    res->curstate = INIT;
    res->creation = ++counter;
    res->sent = 0;
    res->received = 0;
    res->msg_number = 1;
    res->latest_beacon = 0;
    res->next = head;
    head = res;
    FPRINTF(stderr, "DEBUG: (%ld) Driver instance created\n", (unsigned long) driver_connected(port));

    return (ErlDrvData)res;
}

static void drv_stop(ErlDrvData handle) {
    driver_data_t *iterator, *prev = NULL;
    ack_data_t *iterator2, *prev2 = NULL, *tmp;
    driver_data_t *res = (driver_data_t*)handle;
    if (res == beacon_thread.owner) {
        // set the boolean and join the beaconing thread
        erl_drv_mutex_lock(beacon_mutex);
        beaconBool = 0;
        erl_drv_mutex_unlock(beacon_mutex);
        erl_drv_thread_join(beacon_thread.erl_thread, NULL);
    }
    // remove pending acks
    erl_drv_mutex_lock(ack_mutex);
    iterator2 = acks;
    while (iterator2 != NULL) {
        // pointer comparison is fine here
        if (iterator2->res == res) {
            tmp = iterator2;
            if (prev == NULL) {
                acks = iterator2->next;
            }
            else {
                prev2->next = iterator2->next;
            }
            iterator2 = iterator2->next;
            driver_free(tmp);
        }
        else {
            prev2 = iterator2;
            iterator2 = iterator2->next;
        }
    }
    erl_drv_mutex_unlock(ack_mutex);
    // remove the resource
    iterator = head;
    while (iterator != NULL) {
        if (iterator == res) {
            break;
        }
        else {
            prev = iterator;
            iterator = iterator->next;
        }
    }
    if (prev == NULL) {
        head = res->next;
    }
    else {
        prev->next = res->next;
    }
    free_entry(res);
    FPRINTF(stderr, "DEBUG: Driver instance cleared\n");
}

static void drv_finish() {
    driver_data_t *iterator = head, *tmp;
    ip_data_t *iterator2 = peers, *tmp2;
    ack_data_t *iterator3 = acks, *tmp3;
    while (iterator != NULL) {
        tmp = iterator;
        iterator = tmp->next;
        free_entry(tmp);
    }
    while (iterator2 != NULL) {
        tmp2 = iterator2;
        iterator2 = tmp2->next;
        driver_free(tmp2);
    }
    while (iterator3 != NULL) {
        tmp3 = iterator3;
        iterator3 = tmp3->next;
        driver_free(tmp3);
    }
    erl_drv_mutex_destroy(mutex);
    erl_drv_mutex_destroy(ack_mutex);
    close(sock);
    fprintf(stderr, "DEBUG: UDP driver unloaded\n");
}

static void drv_ready_input(ErlDrvData handle, ErlDrvEvent event) {
    // executed *only* by the first port, listening at 4369
    driver_data_t* res = (driver_data_t*)handle;
    FPRINTF(stderr, "DEBUG: (%ld) Input ready\n", (unsigned long) driver_connected(res->port));
    do_recv(res);
}

static void drv_output(ErlDrvData handle, char* buf, ErlDrvSizeT len) {
    driver_data_t* res = (driver_data_t*)handle;
    ip_data_t* tmp;
    int reliable;
    char bufname[BUF];

    if (len == 0)
        return;
    if (res->curstate == HANDSHAKED) {
        reliable = (int) buf[len - 1];
        FPRINTF(stderr, "DEBUG: (%ld) Direct output (%d - %d)\n", (unsigned long) driver_connected(res->port), (int) buf[0], reliable);
        if (reliable) buf[0] = DATA_MSG_ACK_REQUIRED;
        else buf[0] = DATA_MSG;
        do_send2(res, buf, len - 1, reliable);
        return;
    }
    FPRINTF(stderr, "DEBUG: (%ld) Output: %c (%hd)\n", (unsigned long) driver_connected(res->port), (char) *buf, (unsigned short) *buf);
    switch (*buf) {
        case 'L':
            res->curstate = LISTEN;
            driver_output(res->port, "Lok", 3);
            break;
        case 'S':
            res->curstate = SEND;
            buf[0] = DATA_MSG;
            do_send2(res, buf, len, 0);
            break;
        case 'R':
            res->curstate = RECEIVE;
            if (res->peer.sin_addr.s_addr == 0) {
                if (peers == NULL) {
                    FPRINTF(stderr, "DEBUG: Port in receive without a peer\n");
                }
                else {
                    memcpy(&res->peer, &peers->peer, sizeof(struct sockaddr_in));
                    tmp = peers;
                    peers = tmp->next;
                    driver_free(tmp);
                    FPRINTF(stderr, "DEBUG: Found a waiting peer, assigned\n");
                }
            }
            break;
        case 'A':
            res->curstate = ACCEPT;
            break;
        case 'B':
            res->curstate = BEACONING;
            break;
        case 'E':
            res->curstate = CLEANING;
            do_clean(res);
            break;
        case 'C':
            // buffer does not have termination, we need to add it
            memcpy(bufname, buf + 1, len - 1);
            bufname[len - 1] = '\0';
            FPRINTF(stderr, "DEBUG: Connection to %s\n", bufname);
            res->curstate = CONNECT;
            memset(&res->peer, 0, sizeof(struct sockaddr_in));
            res->peer.sin_family = AF_INET;
            res->peer.sin_port = htons(PORT);
            res->peer.sin_addr.s_addr = inet_addr(bufname);
            FPRINTF(stderr, "DEBUG: Address %d\n", res->peer.sin_addr.s_addr);
            driver_output(res->port, "Cok", 3);
            break;
        default:
            FPRINTF(stderr, "DEBUG: Wrong command to UDP driver\n");
            break;
    }
}

static ErlDrvSSizeT drv_control(ErlDrvData handle, unsigned int cmd, char* buf, ErlDrvSizeT size, char** res,
        ErlDrvSizeT res_size)
{
    /* Local macro to ensure large enough buffer. */
#define ENSURE(N)               \
       do {                     \
           if (res_size < N) {          \
           *res = driver_alloc(N);         \
           }                    \
       } while(0)

    driver_data_t *dres = (driver_data_t *)handle;
    char tick = TICK_MSG;
    
    FPRINTF(stderr, "DEBUG: Control: %c\n", (char) cmd);

    switch (cmd) {
        case 'S': {
            ENSURE(13);
            **res = 0;
            put_packet_length((*res) + 1, dres->received);
            put_packet_length((*res) + 5, dres->sent);
            put_packet_length((*res) + 9, driver_sizeq(dres->port));
            return 13;
        }
        case 'T': /* tick */
            do_send2(dres, &tick, 1, 0);
            ENSURE(1);
            **res = 0;
            return 1;
        case 'R':
            ENSURE(2);
            (*res)[0] = 0;
            (*res)[1] = dres->creation;
            return 2;
        case 'D':
            dres->curstate = HANDSHAKED;
            dres->latest_beacon = get_secs();
            ENSURE(1);
            **res = 0;
            return 1;
        case 'P':
            print_ports();
            ENSURE(1);
            **res = 0;
            return 1;
        default:
            return report_control_error(res, res_size, "einval");
    }
#undef ENSURE
}

void drv_timeout(ErlDrvData handle) {
    // This is handled by the first port (as for the main socket)
    ack_data_t *iterator, *prev = NULL, *tmp;
    driver_data_t *res = (driver_data_t *)handle;
    unsigned long now = get_current_time();
    erl_drv_mutex_lock(ack_mutex);
    iterator = acks;
    while (iterator != NULL) {
        if (check_expired(now, iterator->last, iterator->resend)) {
            if (iterator->resend == RESEND_MAX_N) {
                if (prev == NULL) {
                    acks = iterator->next;
                }
                else {
                    prev->next = iterator->next;
                }
                tmp = iterator;
                iterator = iterator->next;
                do_send_upstairs(tmp, res);
                driver_free(tmp);
            }
            else {
                do_resend(iterator);
                //FPRINTF(stderr, "DEBUG: Timer expired for message %d\n", iterator->msg);
                iterator->resend++;
                iterator->last = now;
                prev = iterator;
                iterator = iterator->next;
            }
        }
        else {
            prev = iterator;
            iterator = iterator->next;
        }
    }
    erl_drv_mutex_unlock(ack_mutex);
    driver_set_timer(res->port, RESEND_MIN);
}

/*
 * Helper functions
 */

void do_send_ack(int socket, uint32_t msg, struct sockaddr_in* client) {
    char buf[MINIBUF];
    int size;
    
    buf[0] = ACK_MSG;
    memcpy(buf + 1, (char*) &msg, sizeof(uint32_t));
    size = sendto(socket, buf, 1 + sizeof(uint32_t), 0, (struct sockaddr*) client, sizeof(struct sockaddr_in));
    if (size > 0) FPRINTF(stderr, "DEBUG: Sent ACK for message %d by %d\n", msg, client->sin_addr.s_addr);
    else FPRINTF(stderr, "DEBUG: Unable to send ACK for message %d to %d\n", msg, client->sin_addr.s_addr);
}

void do_resend(ack_data_t* ack) {
    int size = sendto(ack->res->clientSock, ack->buf, ack->len, 0, (struct sockaddr*) &ack->peer, sizeof(struct sockaddr_in));
    if (size <= 0) FPRINTF(stderr, "DEBUG: Unable to resend message %d for the %d turn\n", ack->msg, ack->resend);
    else FPRINTF(stderr, "DEBUG: Message %d resend for the %d turn\n", ack->msg, ack->resend);
}

void do_recv(driver_data_t* res) {
    char buf[BUF], msg_type, bcon[5];
    struct sockaddr_in *client = driver_alloc(sizeof(struct sockaddr_in));
    int size, size2;
    uint32_t msg;
    socklen_t clientSize = sizeof(struct sockaddr_in);
    int existing = 0;
    driver_data_t* iterator = head;
    ip_data_t *peer, *iterator2 = peers;
    ack_data_t *iterator3, *prev = NULL;

    memset(client, 0, sizeof(struct sockaddr_in));
    erl_drv_mutex_lock(mutex);
    FPRINTF(stderr, "DEBUG: Acquired lock for peeking\n");
    size = recvfrom(sock, buf, BUF, MSG_PEEK, (struct sockaddr*)client, &clientSize);
    erl_drv_mutex_unlock(mutex);
    if (size > 0) {
        msg_type = buf[0];
        FPRINTF(stderr, "DEBUG: Peeking %d bytes of type %c from %d:%d through UDP\n", size, msg_type, client->sin_addr.s_addr, client->sin_port);
        // if we have a beacon, let's immediately throw away the content from the socket
        if (msg_type == BEACON_MSG) {
            erl_drv_mutex_lock(mutex);
            recvfrom(sock, buf, 1, 0, (struct sockaddr *) NULL, (socklen_t *) NULL);
            erl_drv_mutex_unlock(mutex);
        }
        while (iterator != NULL) {
            if (iterator->peer.sin_addr.s_addr == client->sin_addr.s_addr && (iterator->curstate == RECEIVE || iterator->curstate == HANDSHAKED)) {
                existing = 1;
                break;
            }
            iterator = iterator->next;
        }
        if (existing) {
            if (msg_type != BEACON_MSG) {
                memset(buf, 0, BUF);
                erl_drv_mutex_lock(mutex);
                FPRINTF(stderr, "DEBUG: Acquired lock for receiving\n");
                size2 = recvfrom(sock, buf, BUF, 0, (struct sockaddr *) NULL, (socklen_t *) NULL);
                erl_drv_mutex_unlock(mutex);
                if (size2 != size) {
                    FPRINTF(stderr, "DEBUG: Received sizes are different: %d vs. %d\n", size, size2);
                }
                else {
                    memcpy((char*) &msg, buf + 1, sizeof(uint32_t));
                    if (msg_type == ACK_MSG) {
                        FPRINTF(stderr, "DEBUG: ACK for message %d\n", msg);
                        erl_drv_mutex_lock(ack_mutex);
                        iterator3 = acks;
                        while (iterator3 != NULL) {
                            // pointers comparison is fine
                            if (iterator3->res == iterator && iterator3->msg == msg) {
                                if (prev == NULL) {
                                    acks = iterator3->next;
                                }
                                else {
                                    prev->next = iterator3->next;
                                }
                                driver_free(iterator3);
                                break;
                            }
                            prev = iterator3;
                            iterator3 = iterator3->next;
                        }
                        erl_drv_mutex_unlock(ack_mutex);
                    }
                    else {
                        FPRINTF(stderr, "DEBUG: Message %d\n", msg);
                        // overwrite the message internal header
                        if (iterator->curstate == HANDSHAKED) {
                            buf[sizeof(uint32_t)] = DIST_MAGIC_RECV_TAG;
                        }
                        else {
                            buf[sizeof(uint32_t)] = 'R';
                        }
                        iterator->received += size2;
                        driver_output(iterator->port, buf + sizeof(uint32_t), msg_type == TICK_MSG ? 0 : size2 - sizeof(uint32_t));
                        if (msg_type == DATA_MSG_ACK_REQUIRED) {
                            client->sin_port = PORT;
                            do_send_ack(iterator->clientSock, msg, client);
                        }
                    }
                }
            }
            else {
                // if beacons come from an already connected peer, let's just update the timing
                iterator->latest_beacon = get_secs();
            }
        }
        else {
            while (iterator2 != NULL) {
                if (iterator2->peer.sin_addr.s_addr == client->sin_addr.s_addr) {
                    FPRINTF(stderr, "DEBUG: Address already waiting to be picked up, ignoring\n");
                    driver_free(client);
                    return;
                }
                iterator2 = iterator2->next;
            }
            if (msg_type == BEACON_MSG) {
                FPRINTF(stderr, "DEBUG: Beacon from an unknown peer\n");
                iterator = head;
                while (iterator != NULL) {
                    if (iterator->curstate == BEACONING) {
                        FPRINTF(stderr, "DEBUG: Routing done\n");
                        // return 'B' + client address (as 32 bit integer)
                        bcon[0] = 'B';
                        memcpy(bcon + 1, &client->sin_addr.s_addr, 4);
                        driver_output(iterator->port, bcon, 5);
                        break;
                    }
                    iterator = iterator->next;
                }
                FPRINTF(stderr, "DEBUG: Beaconing done\n");
                driver_free(client);
                return;
            }
            else {
                FPRINTF(stderr, "DEBUG: Message for a non-existing peer, new connection for accept!\n");
                iterator = head;
                while (iterator != NULL) {
                    if (iterator->curstate == ACCEPT) {
                        FPRINTF(stderr, "DEBUG: Routing done\n");
                        peer = driver_alloc(sizeof(ip_data_t));
                        memcpy(&peer->peer, client, sizeof(struct sockaddr_in));
                        peer->peer.sin_port = htons(PORT);
                        peer->next = peers;
                        peers = peer;
                        driver_output(iterator->port, "Aok", 3);
                        break;
                    }
                }
            }
        }
        driver_free(client);
    }
    else {
        FPRINTF(stderr, "DEBUG: Unable to receive anything (%d - %d)\n", size, errno);
    }
}

int do_send(driver_data_t* res, char* buf, int len, int reliable) {
    int size;

    size = sendto(res->clientSock, buf, len, 0, (struct sockaddr*)&res->peer,
                    sizeof(struct sockaddr_in));
    if (size > 0) {
        res->sent += size;
    }
    FPRINTF(stderr, "DEBUG: Sent message %d (%d bytes) to %d:%d through UDP (%d)\n", res->msg_number, size, res->peer.sin_addr.s_addr, res->peer.sin_port, errno);
    res->msg_number++;
    if (res->curstate != HANDSHAKED) driver_output(res->port, "Sok", 3);
    
    return size;
}

int do_send2(driver_data_t* res, char* buf, int len, int reliable) {
    int size;
    ack_data_t* ack;

    ack = driver_alloc(sizeof(ack_data_t));
    ack->buf[0] = buf[0];
    size = 1;
    memcpy(ack->buf + size, (char*) &res->msg_number, sizeof(uint32_t));
    size += sizeof(uint32_t);
    FPRINTF(stderr, "DEBUG: Minibuf %d, buf %d\n", size, len);
    memcpy(ack->buf + size, buf + 1, len - 1); // drop the initial character, already inserted at the beginning
    size = sendto(res->clientSock, ack->buf, size + len - 1, 0, (struct sockaddr*)&res->peer,
                    sizeof(struct sockaddr_in));
    if (size > 0) {
        res->sent += size;
    }
    FPRINTF(stderr, "DEBUG: Sent message %d (type %d, %d bytes, from %lu) to %d:%d through UDP (%d)\n", res->msg_number, ack->buf[0], size, driver_caller(res->port), res->peer.sin_addr.s_addr, res->peer.sin_port, errno);
    if (res->curstate != HANDSHAKED) driver_output(res->port, "Sok", 3);
    ack->resend = 1;
    ack->len = size;
    ack->last = get_current_time();
    ack->msg = res->msg_number;
    ack->res = res;
    ack->caller = driver_caller(res->port);
    memcpy(&ack->peer, &res->peer, sizeof(struct sockaddr_in));
    if (ack->buf[0] == DATA_MSG_ACK_REQUIRED) {
        erl_drv_mutex_lock(ack_mutex);
        ack->next = acks;
        acks = ack;
        erl_drv_mutex_unlock(ack_mutex);
    }
    else {
        driver_free(ack);
    }
    
    res->msg_number++;
    return size;
}

void do_send_upstairs(ack_data_t *ack, driver_data_t *res) {
    ErlDrvTermData spec[] = {
        ERL_DRV_ATOM, driver_mk_atom("nack"),
        ERL_DRV_BUF2BINARY, (ErlDrvTermData) ack->buf, ack->len,
        ERL_DRV_TUPLE, 2,
    };
    int result = driver_send_term(res->port, ack->caller, spec, sizeof(spec) / sizeof(spec[0]));
    FPRINTF(stderr, "DEBUG: Nack sent upstairs, to %lu, with result %d\n", ack->caller, result);
}

void *do_beacon(void *data) {
    struct sockaddr_in dest;
    char msg = BEACON_MSG;
    int *sockfd = (int *) data;
    int size;
    
    FPRINTF(stderr, "DEBUG: Starting beaconing...\n");
    memset(&dest, 0, sizeof(struct sockaddr_in));
    dest.sin_family = AF_INET;
    dest.sin_port = htons(PORT);
    dest.sin_addr.s_addr = htonl(INADDR_BROADCAST);
    for (;;) {
        // check if we have to quit
        erl_drv_mutex_lock(beacon_mutex);
        if (!beaconBool) {
            erl_drv_mutex_unlock(beacon_mutex);
            break;
        }
        erl_drv_mutex_unlock(beacon_mutex);
        // send the beacon
        size = sendto(*sockfd, &msg, 1, 0, (struct sockaddr *) &dest, sizeof(struct sockaddr_in));
        if (size == 1) FPRINTF(stderr, "DEBUG: Beacon sent\n");
        else FPRINTF(stderr, "DEBUG: Beacon error %d (%d)\n", size, errno);
        // done
        sleep(BEACON_TIMING);
    }
    erl_drv_thread_exit(0);
    
    return NULL;
}

void do_clean(driver_data_t *res) {
    driver_data_t *iterator = head;
    int_data_t *tmp, *list = NULL;
    unsigned long now = get_secs();
    int counter = 0, i = 0;
    char *result;
    
    while (iterator != NULL) {
        if (iterator->curstate == HANDSHAKED) {
            if (now - iterator->latest_beacon > BEACON_TIMEOUT) {
                tmp = (int_data_t *) driver_alloc(sizeof(int_data_t));
                tmp->number = iterator->peer.sin_addr.s_addr;
                tmp->next = list;
                list = tmp;
                ++counter;
            }
        }
        iterator = iterator->next;
    }
    result = (char *) driver_alloc(1 + counter * 4);
    result[0] = 'E';
    result = result + 1;
    if (counter > 0) {
        while (list != NULL) {
            memcpy(result + (4 * i++), &list->number, 4);
            tmp = list;
            list = list->next;
            driver_free(tmp);
        }
    }
    driver_output(res->port, result - 1, 1 + counter * 4);
    driver_free(result - 1);
}

void print_ports() {
    driver_data_t *iterator = head;
    while (iterator != NULL) {
        FPRINTF(stderr, "PORT: connected to %d, status %d\n", iterator->peer.sin_addr.s_addr, iterator->curstate);
        iterator = iterator->next;
    }
}

void print_acks() {
    ack_data_t *iterator = acks;
    while (iterator != NULL) {
        FPRINTF(stderr, "ACK: message %d waiting since %d turns\n", iterator->msg, iterator->resend);
        iterator = iterator->next;
    }
}

int check_expired(unsigned long now, unsigned long last, int resend) {
    int multiplier = pow(EXP_BACKOFF, resend);
    
    return now > last + RESEND_MIN * multiplier;
}

static void put_packet_length(char *b, int len)
{      
    unsigned char *p = (unsigned char *) b;
    unsigned int n = (unsigned int) len;
    p[0] = (n >> 24) & 0xFF;
    p[1] = (n >> 16) & 0xFF;
    p[2] = (n >> 8) & 0xFF;
    p[3] = n & 0xFF;
}

static int report_control_error(char **buffer, int buff_len, 
                char *error_message)
{
    int elen = strlen(error_message);
    if (elen + 1 < buff_len) {
    *buffer = driver_alloc(elen + 1);
    }
    **buffer = 1;
    memcpy((*buffer) + 1, error_message, elen);
    return elen + 1;
}

void free_entry(driver_data_t* element) {
    close(element->clientSock);
    driver_free(element);
}

unsigned long get_current_time() {
    unsigned long res;
    ErlDrvNowData *now = driver_alloc(sizeof(ErlDrvNowData));
    driver_get_now(now);
    //res = now->secs + now->megasecs * 1000000;
    res = now->secs * 1000 + now->microsecs / 1000;
    driver_free(now);
    
    return res;
}

unsigned long get_secs() {
    unsigned long res;
    ErlDrvNowData *now = driver_alloc(sizeof(ErlDrvNowData));
    driver_get_now(now);
    res = now->secs;
    driver_free(now);
    
    return res;
}