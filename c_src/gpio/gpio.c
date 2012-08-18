/*
   Copyright (c) 2012 Alessandro Sivieri <sivieri@elet.polimi.it>

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License version  as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public License
   along with this library; see the file COPYING.LIB.  If not, write to
   the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.
*/

#ifdef __mips__
#include <linux/gpio_dev.h>
#include <linux/ioctl.h>
#endif

#include "gpio.h"

#ifdef __mips__
#define IOCTL(...) ioctl(__VA_ARGS__)
#else
#define IOCTL(...) 0
#define GPIO_DIR_OUT 0
#define GPIO_DIR_IN 0
#define GPIO_GET 0
#define GPIO_SET 0
#define GPIO_CLEAR 0
#endif

int pin_dirout(int fd, int pin) {
    return IOCTL(fd, GPIO_DIR_OUT, pin);
}

int pin_dirin(int fd, int pin) {
    return IOCTL(fd, GPIO_DIR_IN, pin);
}

int pin_get(int fd, int pin) {
    return IOCTL(fd, GPIO_GET, pin);
}

int pin_set(int fd, int pin) {
    return IOCTL(fd, GPIO_SET, pin);
}

int pin_clear(int fd, int pin) {
    return IOCTL(fd, GPIO_CLEAR, pin);
}
