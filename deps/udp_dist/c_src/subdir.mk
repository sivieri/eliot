################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
./eliot_udp.c 

OBJS += \
./eliot_udp.o 

C_DEPS += \
./eliot_udp.d 


# Each subdirectory must supply rules for building sources it contributes
%.o: %.c
	@echo 'Building file: $<'
	@echo 'Invoking: Cross GCC Compiler'
	$(CC) -I/opt/eliotarm/lib/erlang/erts-5.9.3.1/include -I/opt/eliotarm/lib/erlang/lib/erl_interface-3.7.9/include -fPIC -g -Wall -c -fmessage-length=0 -fno-common -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


