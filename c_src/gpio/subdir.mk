################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
./eliot_gpio.c \
./gpio.c \
./io.c 

OBJS += \
./eliot_gpio.o \
./gpio.o \
./io.o 

C_DEPS += \
./eliot_gpio.d \
./gpio.d \
./io.d 


# Each subdirectory must supply rules for building sources it contributes
%.o: %.c
	@echo 'Building file: $<'
	@echo 'Invoking: Cross GCC Compiler'
	gcc -I/home/alex/programmi/eliot/lib/erlang/erts-5.9/include -I/opt/lib/erlang/lib/erl_interface-3.7.6/include -fPIC -g -Wall -c -fmessage-length=0 -fno-common -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


